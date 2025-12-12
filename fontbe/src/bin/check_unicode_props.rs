//! Binary tool to compare Rust unicode_script_extensions with Python ufo2ft
//!
//! This ensures that our Rust implementation matches the behavior of
//! ufo2ft.util.unicodeScriptExtensions
//!
//! The caller is responsible for ensuring a venv is setup and/we are checking
//! the correct version of ufo2ft. This program is intended to be run in CI.

use std::{
    collections::BTreeSet,
    process::{Command, exit},
};

use tinystr::TinyAsciiStr;

/// Call Python to get script extensions for multiple codepoints
fn py_script_extensions_batch(start: u32, end: u32) -> Vec<(u32, BTreeSet<TinyAsciiStr<4>>)> {
    let python_code = format!(
        r#"
from ufo2ft.util import unicodeScriptExtensions

for cp in range({start}, {end} + 1):
    scripts = list(unicodeScriptExtensions(cp))
    print(f"{{cp}}: {{','.join(scripts)}}")
"#,
    );

    let output = Command::new("python3")
        .args(["-c", &python_code])
        .output()
        .expect("failed to execute python");

    if !output.status.success() {
        panic!(
            "python failed, stderr: '{}'",
            String::from_utf8_lossy(&output.stderr)
        )
    }

    let stdout = String::from_utf8(output.stdout).expect("our code, our output");
    let mut results = Vec::new();

    for line in stdout.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let (cp, tail) = line.split_once(": ").expect("all lines contain colon");

        let cp: u32 = cp.trim().parse().expect("failed to parse codepoint");

        let scripts: BTreeSet<TinyAsciiStr<4>> = tail
            .split(',')
            .map(|s| TinyAsciiStr::try_from_str(s.trim()).expect("Invalid script name from Python"))
            .collect();

        results.push((cp, scripts));
    }

    results
}

fn iter_range_chunks(
    mut start: u32,
    end: u32,
    chunk_size: u32,
) -> impl Iterator<Item = (u32, u32)> {
    std::iter::from_fn(move || {
        if start == end {
            return None;
        }

        let next_end = start.saturating_add(chunk_size).min(end);
        let result = (start, next_end);
        start = next_end;

        Some(result)
    })
}

fn main() {
    const BATCH_SIZE: u32 = 200000;

    // Test ALL valid Unicode codepoints (0x0000 to 0x10FFFF, excluding surrogates)
    //the surrogates are  0xD800..=0xDFFF
    let rangea = 0x0..0xD7FF;
    let rangeb = 0xF000..0x10FFFF;

    // Process in batches to avoid command line limits and provide progress updates
    let mut mismatches = Vec::new();
    let total_values = rangea.len() + rangeb.len();

    let mut done = 0;
    for (i, (start, end)) in iter_range_chunks(rangea.start, rangea.end, BATCH_SIZE)
        .chain(iter_range_chunks(rangeb.start, rangeb.end, BATCH_SIZE))
        .enumerate()
    {
        done += end - start;
        if i % 2 == 0 {
            eprintln!(
                "Progress: {done}/{total_values} ({:.1}%)",
                (done as f64 / total_values as f64) * 100.0
            );
        }

        let python_results = py_script_extensions_batch(start, end);

        for (cp, python_scripts) in python_results {
            let rust_scripts: BTreeSet<_> =
                fontbe::features::properties::unicode_script_extensions(cp).collect();

            if rust_scripts != python_scripts {
                mismatches.push((cp, rust_scripts, python_scripts));
            }
        }
    }

    // Print summary
    eprintln!("\nTested {total_values} codepoints",);

    if !mismatches.is_empty() {
        eprintln!("Found {} mismatches", mismatches.len());
        eprintln!("\nFirst 20 mismatches:");
        for (cp, rust_result, python_result) in mismatches.iter().take(20) {
            eprintln!(
                "  U+{cp:04X} ('{}'): Rust: {rust_result:?}, Python: {python_result:?}",
                char::from_u32(*cp).unwrap_or('�'),
            );
        }
        exit(1);
    }

    eprintln!("\n✓ SUCCESS: All {total_values} codepoints match!",);
}
