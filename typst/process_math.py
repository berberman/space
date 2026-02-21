#!/usr/bin/env python3
"""
LLM-generated batch process Typst placeholders in HTML files and replace them with MathML.
"""

import sys
import subprocess
import tempfile
import argparse
import xml.etree.ElementTree as ET
from pathlib import Path

try:
    from bs4 import BeautifulSoup
except ImportError:
    sys.exit("Error: BeautifulSoup is required. Please run: pip install beautifulsoup4")

def apply_stretchy_fixes(mathml_string: str) -> str:
    """Safely disables stretchiness on fences unless they contain 'tall' math elements."""
    try:
        ET.register_namespace('', 'http://www.w3.org/1998/Math/MathML')
        ET.register_namespace('h5', 'http://www.w3.org/1999/xhtml')
        
        wrapped_mathml = f"<dummy>{mathml_string}</dummy>"
        root = ET.fromstring(wrapped_mathml)
        
        tall_tags = {
            'mfrac', 'mtable', 'mroot', 'msqrt', 
            'munderover', 'munder', 'mover', 
            'msup', 'msubsup', 'msub'
        }
        
        # Expanded to support standard brackets and braces
        fences = {'(': ')', '[': ']', '{': '}'}
        
        def get_local_name(tag): return tag.split('}', 1)[-1] if '}' in tag else tag

        def contains_tall(elem):
            if get_local_name(elem.tag) in tall_tags: return True
            return any(contains_tall(child) for child in elem)

        for parent in root.iter():
            children = list(parent)
            i = 0
            while i < len(children):
                child = children[i]
                
                if get_local_name(child.tag) == 'mo' and child.text and child.text.strip() in fences:
                    opening_fence = child.text.strip()
                    closing_fence = fences[opening_fence]
                    
                    needs_stretch = False
                    found_closing = False
                    j = i + 1
                    
                    while j < len(children):
                        sibling = children[j]
                        if get_local_name(sibling.tag) == 'mo' and sibling.text and sibling.text.strip() == closing_fence:
                            found_closing = True
                            break
                        if contains_tall(sibling):
                            needs_stretch = True
                        j += 1
                    
                    if found_closing and not needs_stretch:
                        child.set('stretchy', 'false')
                        children[j].set('stretchy', 'false')
                i += 1
        
        processed = root.text or ""
        for child in root:
            processed += ET.tostring(child, encoding='unicode', method='xml')
        return processed.strip()
        
    except Exception as e:
        print(f"Warning: Failed to apply stretchy fixes: {e}", file=sys.stderr)
        return mathml_string

def batch_compile_typst(unique_formulas: list, script_dir: Path) -> list:
    """Compiles all unique formulas in a single Typst run and extracts raw MathML."""
    if not unique_formulas:
        return []

    template_lines = [
        '#import "mathyml/lib.typ": *',
        '#import "mathyml/prelude.typ": *',
        '#show math.equation: to-mathml',
        ''
    ]

    # Just stack the formulas natively. No string delimiters needed!
    for formula, is_block in unique_formulas:
        if is_block:
            template_lines.append(f"$ {formula} $")
        else:
            template_lines.append(f"${formula}$")

    typst_content = "\n".join(template_lines)
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.typ', dir=script_dir, delete=False) as f:
        typst_file = Path(f.name)
        f.write(typst_content)
        
    try:
        result = subprocess.run(
            ['typst', 'compile', str(typst_file), '--format', 'html', '--features', 'html', '-'],
            capture_output=True, text=True, cwd=str(script_dir), check=False
        )
        if result.returncode != 0:
            raise RuntimeError(f"Typst compilation failed:\n{result.stderr}")
            
        # Parse Typst's generated HTML and strictly extract ONLY the <math> elements
        soup = BeautifulSoup(result.stdout, 'html.parser')
        math_tags = soup.find_all('math')
        
        if len(math_tags) != len(unique_formulas):
             raise RuntimeError(f"Mismatch: Sent {len(unique_formulas)} formulas, but Typst returned {len(math_tags)} <math> tags.")
        
        # Apply the stretchy fixes directly to the isolated, clean XML math strings
        processed_blocks = []
        for tag in math_tags:
            raw_mathml = str(tag)
            processed_blocks.append(apply_stretchy_fixes(raw_mathml))
            
        return processed_blocks
        
    finally:
        if typst_file.exists():
            typst_file.unlink()

def main():
    parser = argparse.ArgumentParser(description='Batch replace Typst placeholders in HTML files.')
    parser.add_argument('directory', help='Directory containing HTML files to process')
    args = parser.parse_args()

    site_dir = Path(args.directory).resolve()
    if not site_dir.is_dir():
        sys.exit(f"Error: {site_dir} is not a valid directory.")

    html_files = list(site_dir.rglob("*.html"))
    print(f"Scanning {len(html_files)} HTML files for Typst placeholders...")

    formula_cache = {}
    unique_formulas = []
    
    for html_file in html_files:
        with open(html_file, 'r', encoding='utf-8') as f:
            soup = BeautifulSoup(f, 'html.parser')
            
        for tag in soup.find_all(class_=['typst-inline', 'typst-block']):
            is_block = 'typst-block' in tag.get('class', [])
            formula = tag.get_text().strip()
            key = (formula, is_block)
            
            if key not in formula_cache:
                formula_cache[key] = ""
                unique_formulas.append(key)

    if not unique_formulas:
        print("No Typst placeholders found. Exiting.")
        return

    print(f"Found {len(unique_formulas)} unique formulas. Compiling in batch...")
    
    script_dir = Path(__file__).parent.resolve()
    rendered_blocks = batch_compile_typst(unique_formulas, script_dir)

    for i, key in enumerate(unique_formulas):
        formula_cache[key] = rendered_blocks[i]

    print("Compilation complete. Replacing placeholders in HTML...")

    modified_count = 0
    for html_file in html_files:
        with open(html_file, 'r', encoding='utf-8') as f:
            soup = BeautifulSoup(f, 'html.parser')
            
        modified = False
        for tag in soup.find_all(class_=['typst-inline', 'typst-block']):
            is_block = 'typst-block' in tag.get('class', [])
            formula = tag.get_text().strip()
            key = (formula, is_block)
            
            mathml_str = formula_cache.get(key)
            if mathml_str:
                new_tag = BeautifulSoup(mathml_str, 'html.parser')
                tag.replace_with(new_tag)
                modified = True
                
        if modified:
            with open(html_file, 'w', encoding='utf-8') as f:
                f.write(str(soup))
            modified_count += 1

    print(f"Done! Updated math in {modified_count} HTML files.")

if __name__ == '__main__':
    main()
