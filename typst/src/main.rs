use std::{
    collections::HashMap,
    io::{self, BufRead, BufReader, BufWriter, Write},
    ops::Range,
    path::PathBuf,
};

use serde::{Deserialize, Serialize};

use typst::{
    Library, LibraryExt, World, WorldExt, diag::{FileError, FileResult, SourceDiagnostic}, foundations::{Bytes, Datetime, Duration}, syntax::{FileId, Source, VirtualRoot}, text::{Font, FontBook, FontInfo}, utils::LazyHash,
};

use typst_kit::{
    downloader::SystemDownloader,
    files::{FileLoader, FileStore},
    packages::SystemPackages,
};

use typst_layout::PagedDocument;
use typst_svg::SvgOptions;

#[derive(Debug, Deserialize)]
struct Request {
    id: u64,
    source: String,
    display: bool,
}

#[derive(Debug, Clone, Serialize)]
struct Diagnostic {
    /// "error" or "warning".
    kind: String,

    message: String,

    /// Byte offsets into Request.source.
    ///
    /// These are intentionally not offsets into our synthetic Typst document.
    start: Option<usize>,
    stop: Option<usize>,

    hints: Vec<String>,
}

impl Diagnostic {
    fn error(message: impl Into<String>) -> Self {
        Self {
            kind: "error".to_owned(),
            message: message.into(),
            start: None,
            stop: None,
            hints: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct CompiledFragment {
    svg: Option<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Serialize)]
struct Response {
    id: u64,
    svg: Option<String>,
    diagnostics: Vec<Diagnostic>,
}

impl Response {
    fn internal_error(id: u64, message: impl Into<String>) -> Self {
        Self {
            id,
            svg: None,
            diagnostics: vec![Diagnostic::error(message)],
        }
    }

    fn from_compiled(id: u64, compiled: &CompiledFragment) -> Self {
        Self {
            id,
            svg: compiled.svg.clone(),
            diagnostics: compiled.diagnostics.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    source: String,
    display: bool,
}

const TEXT_SIZE_PT: f64 = 11.0;
const PAGE_PREAMBLE: &str = r#"
#import "@preview/commute:0.3.0": *
#import "@preview/curryst:0.6.0": *

#set text(size: 11pt)

#set page(
  width: auto,
  height: auto,
  margin: 0pt,
  fill: none,
)
"#;
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
                // `self.source`, which MathWorld handles before reaching here.
                Err(FileError::NotFound(PathBuf::from(
                    id.vpath().get_without_slash(),
                )))
            }
        }
    }
}

struct MathWorld {
    library: LazyHash<Library>,
    book: LazyHash<FontBook>,
    fonts: Vec<Font>,
    source: Source,
    package_files: FileStore<PackageFiles>,
}
impl MathWorld {
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

    fn set_formula(&mut self, source: &str, display: bool) -> usize {
        let (open, close) = if display { ("$ ", " $") } else { ("$", "$") };

        let mut wrapped =
            String::with_capacity(PAGE_PREAMBLE.len() + open.len() + source.len() + close.len());

        wrapped.push_str(PAGE_PREAMBLE);
        wrapped.push_str(open);

        let source_start = wrapped.len();

        wrapped.push_str(source);
        wrapped.push_str(close);

        self.source.replace(&wrapped);

        source_start
    }
}

impl World for MathWorld {
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

fn translate_range(
    range: Range<usize>,
    source_start: usize,
    source_len: usize,
) -> Option<(usize, usize)> {
    let source_stop = source_start + source_len;

    if range.start == range.end {
        if range.start >= source_start && range.start <= source_stop {
            let pos = range.start - source_start;
            return Some((pos, pos));
        }

        return None;
    }

    if range.end <= source_start || range.start >= source_stop {
        return None;
    }

    let start = range.start.max(source_start) - source_start;
    let stop = range.end.min(source_stop) - source_start;

    Some((start, stop))
}

fn convert_diagnostic(
    world: &MathWorld,
    diagnostic: &SourceDiagnostic,
    kind: &str,
    source_start: usize,
    source_len: usize,
) -> Diagnostic {
    let range = if diagnostic.span.id() == Some(world.main()) {
        world
            .range(diagnostic.span)
            .and_then(|range| translate_range(range, source_start, source_len))
    } else {
        None
    };

    let (start, stop) = match range {
        Some((start, stop)) => (Some(start), Some(stop)),

        None => (None, None),
    };

    Diagnostic {
        kind: kind.to_owned(),
        message: diagnostic.message.to_string(),
        start,
        stop,

        hints: diagnostic
            .hints
            .iter()
            .map(|hint| hint.v.to_string())
            .collect(),
    }
}

impl MathWorld {
    fn compile_fragment(&mut self, source: &str, display: bool) -> CompiledFragment {
        let source_start = self.set_formula(source, display);

        let warned = typst::compile::<PagedDocument>(&*self);

        // Warnings coexist with successful output.
        let mut diagnostics: Vec<Diagnostic> = warned
            .warnings
            .iter()
            .map(|diagnostic| {
                convert_diagnostic(self, diagnostic, "warning", source_start, source.len())
            })
            .collect();

        let document = match warned.output {
            Ok(document) => document,

            Err(errors) => {
                diagnostics.extend(errors.iter().map(|diagnostic| {
                    convert_diagnostic(self, diagnostic, "error", source_start, source.len())
                }));

                return CompiledFragment {
                    svg: None,
                    diagnostics,
                };
            }
        };

        let pages = document.pages();

        if pages.len() != 1 {
            diagnostics.push(Diagnostic::error(format!(
                "expected Typst to produce exactly one page, got {}",
                pages.len(),
            )));

            return CompiledFragment {
                svg: None,
                diagnostics,
            };
        }

        let page = &pages[0];
        let svg = typst_svg::svg(page, &SvgOptions::default());
        let svg = make_svg_embeddable(svg, page);

        CompiledFragment {
            svg: Some(svg),
            diagnostics,
        }
    }
}

fn make_svg_embeddable(
    mut svg: String,
    page: &typst_layout::Page,
) -> String {
    let width_em =
        page.frame.width().to_pt() / TEXT_SIZE_PT;

    let height_em =
        page.frame.height().to_pt() / TEXT_SIZE_PT;

    let Some(pos) = svg.find("<svg") else {
        return svg;
    };

    let insert_at = pos + "<svg".len();

    svg.insert_str(
        insert_at,
        &format!(
            r#" style="overflow: visible; width: {width_em}em; height: {height_em}em;""#
        ),
    );

    svg
}


struct Server {
    world: MathWorld,
    cache: HashMap<CacheKey, CompiledFragment>,
}

impl Server {
    fn new() -> Self {
        Self {
            world: MathWorld::new(),
            cache: HashMap::new(),
        }
    }

    fn handle(&mut self, request: Request) -> Response {
        let Request {
            id,
            source,
            display,
        } = request;

        let key = CacheKey {
            source: source.clone(),
            display,
        };

        if let Some(cached) = self.cache.get(&key) {
            return Response::from_compiled(id, cached);
        }

        let compiled = self.world.compile_fragment(&source, display);

        // Since this is a long-running process we don't want old memoized
        // generations to accumulate forever
        typst::comemo::evict(30);

        let response = Response::from_compiled(id, &compiled);

        // Cache both successful SVGs and deterministic diagnostics.
        self.cache.insert(key, compiled);

        response
    }
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

    let mut server = Server::new();

    for line in reader.lines() {
        let line = line?;

        if line.trim().is_empty() {
            continue;
        }

        let response = match serde_json::from_str::<Request>(&line) {
            Ok(request) => server.handle(request),

            Err(error) => {
                Response::internal_error(0, format!("invalid request JSON: {error}"))
            }
        };

        write_response(&mut writer, &response)?;
    }

    Ok(())
}
