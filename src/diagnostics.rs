use ariadne::{Color, Label, Report, ReportKind, sources};

use crate::error::Error;
use crate::interpreter::RuntimeError;
use crate::parser::{ParseError, ParseErrors};
use crate::scanner::{ScanError, ScanErrors};
use crate::token::Span;

pub fn render_error(err: Error, source: &str, file_name: &str) -> String {
    match err {
        Error::Scan(ScanErrors(_, errs)) => render_scan_errors(&errs, source, file_name),
        Error::Parse(ParseErrors(_, errs)) => render_parse_errors(&errs, source, file_name),
        Error::Runtime(rt) => render_runtime_errors(rt, source, file_name),
    }
}

fn render_scan_errors(errors: &[ScanError], source: &str, file_name: &str) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    for e in errors {
        let clr = Color::Red;
        let s = e.span();
        let (start, end) = (s.start, s.end);
        let mut buf = Vec::new();
        let msg = e.to_string();
        Report::build(
            ReportKind::Custom("Scan error", Color::Red),
            (file_id, start..end),
        )
        .with_message(msg)
        .with_label(
            Label::new((file_id, start..end))
                .with_message("here")
                .with_color(clr),
        )
        .finish()
        .write(sources([(file_id, source)]), &mut buf)
        .ok();
        let rendered = String::from_utf8_lossy(&buf).into_owned();
        out.push('\n');
        out.push_str(&rendered);
    }
    out
}

fn render_parse_errors(errors: &[ParseError], source: &str, file_name: &str) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    for e in errors {
        let clr = Color::Yellow;
        let (start, end) = e
            .primary_span()
            .unwrap_or_else(|| Span::line_bounds(source, e.line()));
        let mut buf = Vec::new();
        let msg = e.to_string();
        Report::build(
            ReportKind::Custom("Syntax error", Color::Yellow),
            (file_id, start..end),
        )
        .with_message(msg)
        .with_label(
            Label::new((file_id, start..end))
                .with_message("insert token here")
                .with_color(clr),
        )
        .finish()
        .write(sources([(file_id, source)]), &mut buf)
        .ok();
        let rendered = String::from_utf8_lossy(&buf).into_owned();
        out.push('\n');
        out.push_str(&rendered);
    }
    out
}

fn render_runtime_errors(err: RuntimeError, source: &str, file_name: &str) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    let clr = Color::Magenta;
    let msg = err.to_string();
    let mut buf = Vec::new();
    if let Some(span) = err.span() {
        let (start, end) = (span.start, span.end);
        Report::build(
            ReportKind::Custom("Runtime error", Color::Magenta),
            (file_id, start..end),
        )
        .with_message(msg)
        .with_label(
            Label::new((file_id, start..end))
                .with_message("here")
                .with_color(clr),
        )
        .finish()
        .write(sources([(file_id, source)]), &mut buf)
        .ok();
    } else {
        Report::build(ReportKind::Error, (file_id, 0..0))
            .with_message(msg)
            .finish()
            .write(sources([(file_id, source)]), &mut buf)
            .ok();
    }
    let rendered = String::from_utf8_lossy(&buf).into_owned();
    out.push('\n');
    out.push_str(&rendered);
    out
}
