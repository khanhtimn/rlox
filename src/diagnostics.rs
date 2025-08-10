use ariadne::{Color, Label, Report, ReportKind, sources};

use crate::error::Error;
use crate::token::Span;

pub fn render_error(err: Error, source: &str, file_name: &str) -> String {
    match err {
        Error::Scan(crate::scanner::ScanErrors(_, errs)) => {
            render_scan_errors(&errs, source, file_name)
        }
        Error::Parse(crate::parser::ParseErrors(_, errs)) => {
            render_parse_errors(&errs, source, file_name)
        }
        other => render_runtime_errors(other, source, file_name),
    }
}

fn render_scan_errors(
    errors: &[crate::scanner::ScanError],
    source: &str,
    file_name: &str,
) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    for e in errors {
        let clr = Color::Red;
        let s = e.span();
        let (start, end) = (s.start, s.end);
        let mut buf = Vec::new();
        Report::build(ReportKind::Error, (file_id, start..end))
            .with_message(e.to_string())
            .with_label(
                Label::new((file_id, start..end))
                    .with_message("here")
                    .with_color(clr),
            )
            .finish()
            .write(sources([(file_id, source)]), &mut buf)
            .ok();
        out.push('\n');
        out.push_str(&String::from_utf8_lossy(&buf));
    }
    out
}

fn render_parse_errors(
    errors: &[crate::parser::ParseError],
    source: &str,
    file_name: &str,
) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    for e in errors {
        let clr = Color::Yellow;
        let (start, end) = e
            .primary_span()
            .unwrap_or_else(|| Span::line_bounds(source, e.line()));
        let mut buf = Vec::new();
        Report::build(ReportKind::Error, (file_id, start..end))
            .with_message(e.to_string())
            .with_label(
                Label::new((file_id, start..end))
                    .with_message("here")
                    .with_color(clr),
            )
            .finish()
            .write(sources([(file_id, source)]), &mut buf)
            .ok();
        out.push('\n');
        out.push_str(&String::from_utf8_lossy(&buf));
    }
    out
}

fn render_runtime_errors(err: Error, source: &str, file_name: &str) -> String {
    let mut out = String::new();
    let file_id: &'static str = Box::leak(file_name.to_string().into_boxed_str());
    match err {
        Error::Runtime(rt) => {
            let clr = Color::Magenta;
            let (start, end) = rt.span().map(|s| (s.start, s.end)).unwrap_or((0, 0));
            let msg = rt.to_string();
            let mut buf = Vec::new();
            Report::build(ReportKind::Error, (file_id, start..end))
                .with_message(msg)
                .with_label(
                    Label::new((file_id, start..end))
                        .with_message("here")
                        .with_color(clr),
                )
                .finish()
                .write(sources([(file_id, source)]), &mut buf)
                .ok();
            out.push('\n');
            out.push_str(&String::from_utf8_lossy(&buf));
        }
        _ => out.push_str(&err.to_string()),
    }
    out
}
