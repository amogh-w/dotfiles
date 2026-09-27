function md_to_pdf --description "Convert a markdown file to a styled PDF using md-to-pdf"
    if test (count $argv) -lt 1
        echo "Usage: md_to_pdf <name.md>"
        return 1
    end

    set -l input $argv[1]

    if not test -f $input
        echo "File not found: $input"
        return 1
    end

    set -l output (string replace -r '\.md$' '.pdf' $input)

    cat $input | npx md-to-pdf --css "
    @page {
      margin: 18mm 16mm;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      font-size: 10.5pt;
      line-height: 1.65;
      color: #181825;
      font-weight: 500;
      -webkit-font-smoothing: antialiased;
    }

    h1 {
      font-size: 20pt;
      font-weight: 700;
      color: #1e1e2e;
      border-bottom: 3px solid #7287fd;
      padding-bottom: 6px;
      margin-top: 20px;
    }
    h2 {
      font-size: 14pt;
      font-weight: 700;
      color: #313244;
      border-bottom: 1.5px solid #b4befe;
      padding-bottom: 4px;
      margin-top: 22px;
    }
    h3 {
      font-size: 11.5pt;
      font-weight: 600;
      color: #45475a;
      margin-top: 16px;
    }

    strong {
      color: #11111b;
      font-weight: 700;
    }
    td {
      color: #181825;
    }
    tr:nth-child(even) td {
      background-color: #f5f5f9;
    }

    blockquote {
      border-left: 4px solid #7287fd;
      background: #f0f1ff;
      margin: 16px 0;
      padding: 10px 16px;
      color: #313244;
      font-weight: 500;
    }
    pre {
      background-color: #1e1e2e;
      color: #cdd6f4;
      padding: 14px;
      border-radius: 6px;
      font-family: 'SF Mono', Monaco, Menlo, Consolas, monospace;
      font-size: 8.5pt;
      line-height: 1.45;
    }
    code {
      font-family: 'SF Mono', Monaco, Menlo, Consolas, monospace;
      background-color: #e6e9ef;
      color: #181825;
      padding: 2px 5px;
      border-radius: 4px;
      font-size: 9pt;
      font-weight: 600;
    }
  " > $output

    echo "Created $output"
end
