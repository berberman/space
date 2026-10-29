use std::{
    io::{self, BufRead, BufReader, BufWriter, Write},
    path::PathBuf,
};

use serde::{Deserialize, Serialize};

use typst::{
    Library, LibraryExt, World, WorldExt,
    diag::{FileError, FileResult, SourceDiagnostic},
    foundations::{Bytes, Datetime, Duration, Label, Smart},
    layout::{Frame, FrameItem},
    syntax::{FileId, Source, VirtualRoot},
    text::{Font, FontBook, FontInfo},
    utils::{LazyHash, PicoStr},
};

use typst_kit::{
    downloader::SystemDownloader,
    files::{FileLoader, FileStore},
    packages::SystemPackages,
};

use typst_layout::{Page, PagedDocument};
use typst_svg::SvgOptions;

#[derive(Debug, Deserialize)]
struct Request {
    id: u64,
    source: String,
    fragments: u64,
}

#[derive(Debug, Serialize)]
struct Diagnostic {
    /// "error" or "warning".
    kind: String,

    message: String,

    /// Byte offsets into Request.source.
    start: Option<usize>,
    stop: Option<usize>,
    fragment: Option<u64>,

    hints: Vec<String>,
}

impl Diagnostic {
    fn error(message: impl Into<String>) -> Self {
        Self {
            kind: "error".to_owned(),
            message: message.into(),
            start: None,
            stop: None,
            fragment: None,
            hints: Vec::new(),
        }
    }
}

#[derive(Debug, Serialize)]
struct Response {
    id: u64,
    success: bool,
    outputs: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

impl Response {
    fn internal_error(id: u64, message: impl Into<String>) -> Self {
        Self {
            id,
            success: false,
            outputs: Vec::new(),
            diagnostics: vec![Diagnostic::error(message)],
        }
    }
}

const TEXT_SIZE_PT: f64 = 11.0;
struct PackageFiles {
    packages: SystemPackages,
}

impl PackageFiles {
    fn new() -> Self {
        let downloader = SystemDownloader::new("space-typst/0.1.0");

        Self {
            packages: SystemPackages::new(downloader),
        }
    }
}

impl FileLoader for PackageFiles {
    fn load(&self, id: FileId) -> FileResult<Bytes> {
        match id.root() {
            VirtualRoot::Package(spec) => {
                let root = self.packages.obtain(spec)?;
                root.load(id.vpath())
            }

            VirtualRoot::Project => {
                // The only project source we support is the synthetic
                // `self.source`, which FragmentWorld handles before reaching here.
                Err(FileError::NotFound(PathBuf::from(
                    id.vpath().get_without_slash(),
                )))
            }
        }
    }
}

struct FragmentWorld {
    library: LazyHash<Library>,
    book: LazyHash<FontBook>,
    fonts: Vec<Font>,
    source: Source,
    package_files: FileStore<PackageFiles>,
}
impl FragmentWorld {
    fn new() -> Self {
        let source = Source::detached(String::new());

        let (fonts, font_infos): (Vec<Font>, Vec<FontInfo>) = typst_kit::fonts::embedded().unzip();

        let book = FontBook::from_infos(font_infos);

        let library = Library::default();

        let package_files = FileStore::new(PackageFiles::new());
        Self {
            library: LazyHash::new(library),
            book: LazyHash::new(book),
            fonts,
            source,
            package_files,
        }
    }

    fn set_source(&mut self, source: &str) {
        self.source.replace(source);
    }
}

impl World for FragmentWorld {
    fn library(&self) -> &LazyHash<Library> {
        &self.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        &self.book
    }

    fn main(&self) -> FileId {
        self.source.id()
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.source.id() {
            Ok(self.source.clone())
        } else {
            self.package_files.source(id)
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        if id == self.source.id() {
            Ok(Bytes::from_string(self.source.clone()))
        } else {
            self.package_files.file(id)
        }
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.fonts.get(index).cloned()
    }

    fn today(&self, _offset: Option<Duration>) -> Option<Datetime> {
        None
    }
}

fn convert_diagnostic(
    world: &FragmentWorld,
    diagnostic: &SourceDiagnostic,
    kind: &str,
) -> Diagnostic {
    let range = if diagnostic.span.id() == Some(world.main()) {
        world.range(diagnostic.span)
    } else {
        None
    };

    let (start, stop) = match range {
        Some(range) => (Some(range.start), Some(range.end)),

        None => (None, None),
    };

    Diagnostic {
        kind: kind.to_owned(),
        message: diagnostic.message.to_string(),
        start,
        stop,
        fragment: None,

        hints: diagnostic
            .hints
            .iter()
            .map(|hint| hint.v.to_string())
            .collect(),
    }
}

impl FragmentWorld {
    fn compile_document(&mut self, request: Request) -> Response {
        let Request {
            id,
            source,
            fragments,
        } = request;
        self.set_source(&source);
        let warned = typst::compile::<PagedDocument>(&*self);
        let mut diagnostics: Vec<Diagnostic> = warned
            .warnings
            .iter()
            .map(|diagnostic| convert_diagnostic(self, diagnostic, "warning"))
            .collect();

        let document = match warned.output {
            Ok(document) => document,
            Err(errors) => {
                diagnostics.extend(
                    errors
                        .iter()
                        .map(|diagnostic| convert_diagnostic(self, diagnostic, "error")),
                );
                return Response {
                    id,
                    success: false,
                    outputs: Vec::new(),
                    diagnostics,
                };
            }
        };

        let mut pages = Vec::with_capacity(fragments as usize);
        for fragment in 0..fragments {
            match fragment_page(&document, fragment) {
                Ok(page) => pages.push(page),
                Err(message) => {
                    let mut diagnostic = Diagnostic::error(message);
                    diagnostic.fragment = Some(fragment);
                    diagnostics.push(diagnostic);
                }
            }
        }

        if diagnostics
            .iter()
            .any(|diagnostic| diagnostic.kind == "error")
        {
            return Response {
                id,
                success: false,
                outputs: Vec::new(),
                diagnostics,
            };
        }

        let outputs = pages
            .into_iter()
            .map(|page| {
                let has_background = matches!(&page.fill, Smart::Auto | Smart::Custom(Some(_)));
                if has_background || frame_has_visible_output(&page.frame) {
                    make_svg_embeddable(typst_svg::svg(page, &SvgOptions::default()), page)
                } else {
                    String::new()
                }
            })
            .collect();

        Response {
            id,
            success: true,
            outputs,
            diagnostics,
        }
    }
}

fn fragment_page(document: &PagedDocument, fragment: u64) -> Result<&Page, String> {
    let introspector = document.introspector();
    let marker_page = |boundary: &str| -> Result<usize, String> {
        let name = format!("__space-typst-{boundary}-{fragment}");
        let marker = introspector
            .elements()
            .query_label(
                Label::new(PicoStr::intern(&name)).expect("fragment marker label is nonempty"),
            )
            .map_err(|error| {
                format!("cannot find unique {boundary} marker for fragment {fragment}: {error}")
            })?;
        let location = marker
            .location()
            .ok_or_else(|| format!("{boundary} marker for fragment {fragment} has no location"))?;
        let position = introspector.position(location).ok_or_else(|| {
            format!("{boundary} marker for fragment {fragment} has no page position")
        })?;
        Ok(position.page.get() - 1)
    };
    let start = marker_page("start")?;
    let end = marker_page("end")?;
    if end < start {
        return Err(format!(
            "end marker precedes start marker for fragment {fragment}"
        ));
    }
    if start != end {
        return Err(format!(
            "expected Typst to produce exactly one page for fragment {fragment}, got {}",
            end - start + 1,
        ));
    }
    document
        .pages()
        .get(start)
        .ok_or_else(|| format!("fragment {fragment} markers point outside the document pages"))
}

fn frame_has_visible_output(frame: &Frame) -> bool {
    frame.items().any(|(_, item)| match item {
        FrameItem::Group(group) => frame_has_visible_output(&group.frame),
        FrameItem::Tag(_) | FrameItem::Link(..) => false,
        FrameItem::Text(text) => !text.glyphs.is_empty(),
        FrameItem::Shape(..) | FrameItem::Image(..) => true,
    })
}

fn make_svg_embeddable(svg: String, page: &Page) -> String {
    let width_em = page.frame.width().to_pt() / TEXT_SIZE_PT;

    let height_em = page.frame.height().to_pt() / TEXT_SIZE_PT;

    let Some(pos) = svg.find("<svg") else {
        return svg;
    };

    let insert_at = pos + "<svg".len();
    let mut svg = svg
        .replace("fill=\"#000000\"", "fill=\"currentColor\"")
        .replace("stroke=\"#000000\"", "stroke=\"currentColor\"");

    svg.insert_str(
        insert_at,
        &format!(r#" style="overflow: visible; width: {width_em}em; height: {height_em}em;""#),
    );

    svg
}

fn write_response(writer: &mut impl Write, response: &Response) -> io::Result<()> {
    // stdout is the protocol!!
    serde_json::to_writer(&mut *writer, response).map_err(io::Error::other)?;

    writer.write_all(b"\n")?;

    // Lean waits synchronously for one response line, so flushing after each
    // response is required.
    writer.flush()
}

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();

    let reader = BufReader::new(stdin.lock());

    let mut writer = BufWriter::new(stdout.lock());

    let mut world = FragmentWorld::new();

    for line in reader.lines() {
        let line = line?;

        if line.trim().is_empty() {
            continue;
        }

        let response = match serde_json::from_str::<Request>(&line) {
            Ok(request) => {
                let response = world.compile_document(request);
                typst::comemo::evict(30);
                response
            }

            Err(error) => Response::internal_error(0, format!("invalid request JSON: {error}")),
        };

        write_response(&mut writer, &response)?;
    }

    Ok(())
}
