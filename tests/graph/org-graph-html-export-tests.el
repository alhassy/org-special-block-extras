;;; org-graph-html-export-tests.el --- HTML export  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; HTML export

(deftest "`#+begin_graph' :svg nil HTML export produces an <img> tag"
  ;; Call the graph block handler directly: simulate what org-export-string
  ;; does for a #+begin_graph block, bypassing the full export pipeline
  ;; (which triggers org-defblock docstring enrichment and breaks in batch).
  (cl-letf (((symbol-function 'org-graph--render)
             (lambda (_dot _kind pngfile)
               ;; Write a minimal 1×1 white PNG so base64 encoding succeeds.
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "
\x89PNG\r
\x1a"))
               t)))
    (let ((html (org-block/graph 'html "
+ A
  + B
    | :edge.label | calls |"
                                 nil :svg nil)))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' HTML export: node and edge appear in underlying dot"
  ;; Verify the dot source that gets embedded is structurally correct
  ;; by checking the raw->dot output directly.
  (let ((dot (org--graph-raw->dot
              "
+ A
  + B
    | :edge.label | calls |"
              nil)))
    (should (s-contains-p "A -> B [ label=calls ];" dot))
    (should (s-contains-p "digraph" dot))))

(deftest "`#+begin_graph' :svg t — HTML export embeds inline <svg>"
  ;; With :svg t the handler calls org-graph--render and returns
  ;; the raw <svg>…</svg> string for inline embedding (no base64, no <img>).
  (cl-letf (((symbol-function 'org-graph--render)
             (lambda (_dot _kind svgfile)
               (with-temp-file svgfile
                 (insert "
<?xml version=\"1.0\"?>
<!DOCTYPE svg>
<svg xmlns=\"http://www.w3.org/2000/svg\"><g><title>A</title></g></svg>"))
               svgfile)))
    (let ((html (org-block/graph 'html "
+ A" nil :svg t)))
      (should (s-contains-p "<svg" html))
      ;; Graphviz's <title>s would hover natively, competing with our tooltips.
      (should-not (s-contains-p "<title>" html))
      (should-not (s-contains-p "<img" html))
      (should-not (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' :svg t — XML preamble is stripped"
  ;; org-graph--svg-contents must strip the <?xml …?> and <!DOCTYPE> lines
  ;; so only the <svg>…</svg> element is embedded in the HTML.
  (cl-letf (((symbol-function 'org-graph--render)
             (lambda (_dot _kind svgfile)
               (with-temp-file svgfile
                 (insert "
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"\">
<svg xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>"))
               svgfile)))
    (let ((html (org-block/graph 'html "
+ A" nil :svg t)))
      (should-not (s-contains-p "<?xml" html))
      (should-not (s-contains-p "<!DOCTYPE" html))
      (should (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' :svg t — falls back to <pre> when dot unavailable"
  ;; If org-graph--render returns nil (dot not installed), the
  ;; handler must fall back to a <pre> block with escaped dot source.
  (cl-letf (((symbol-function 'org-graph--render)
             (lambda (_dot _kind _svgfile) nil)))
    (let ((html (org-block/graph 'html "
+ A
  + B" nil :svg t)))
      (should (s-contains-p "<pre>" html))
      (should-not (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' :svg nil — HTML export produces a base64 PNG"
  (cl-letf (((symbol-function 'org-graph--render)
             (lambda (_dot _kind pngfile)
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "
\x89PNG\r
\x1a"))
               t)))
    (let ((html (org-block/graph 'html "
+ A" nil :svg nil)))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html))
      (should-not (s-contains-p "<svg" html)))))

;;; Tooltips: an inline <svg>'s hover by tooltips.js, as prose's do

(deftest "`#+begin_graph' HTML export inlines an <svg> sans :svg, so its tooltips hover"
  (let (kind)
    (cl-letf (((symbol-function 'org-graph--render)
               (lambda (_dot k file) (setq kind k) (with-temp-file file (insert "<svg width=\"1\"></svg>")) file)))
      (should (s-contains-p "<svg width=\"1\"></svg>" (org-block/graph 'html "
+ A" nil)))
      (should (equal "svg" kind)))))

(deftest "Collecting tooltips, a graph's dot holds their keys, and its tooltips stay Org, markup and all"
  (let* ((org--graph-tips (make-hash-table :test #'equal))
         (dot (org--graph-raw->dot "
+ Church [fn::Born /1903/]
  + Turing
    #+begin_note :edge
    His *doctoral* advisor.
    #+end_note" nil)))
    (should-not (s-contains-p "1903" dot))
    (should (equal '("Born /1903/" "His *doctoral* advisor.") (-sort #'string< (hash-table-values org--graph-tips))))
    (maphash (lambda (key _) (should (s-contains-p (format "tooltip=\"%s\"" key) dot))) org--graph-tips)))

(deftest "Outside an inline <svg>'s export, a graph's tooltips are plain text, as dot shows them"
  (should (s-contains-p "tooltip=\"Born 1903\"" (org--graph-raw->dot "
+ Church [fn::Born /1903/]" nil))))

(deftest "An <svg>'s keyed titles become tooltips.js's tooltips, their <template>s after it; Graphviz's own titles vanish"
  (let ((tips (make-hash-table :test #'equal)))
    (puthash "osbe-tip-g1" "Born /1903/" tips)
    (should (equal (org-graph--svg-tooltips "<svg><title>%3</title>
<g class=\"node\"><title>Church</title>
<a xlink:title=\"osbe&#45;tip&#45;g1\">Church</a>
<a xlink:href=\"https://x.org\" xlink:title=\"Turing\">Turing</a></g></svg>" tips)
                   "<svg><g class=\"node\"><a class=\"tooltip osbe-tip-g1\" tabindex=\"0\">Church</a>
<a xlink:href=\"https://x.org\">Turing</a></g></svg>
<template id=\"osbe-tip-g1\"><p>
Born <i>1903</i></p></template>"))))

;;; Markdown export

(deftest "`#+begin_graph' Markdown export writes the :file image and links it, as its renderers strip base64 <img>s"
  (require 'ox-md)
  (let (written)
    (cl-letf (((symbol-function 'org-graph--render) (lambda (_dot _kind file) (setq written file) t)))
      ;; The header reads :file foo.png as the symbol foo.png.
      (let* ((org-export-current-backend 'md)
             (md (org-block/graph 'html "
+ A" nil :file 'foo.png)))
        (should (equal written "foo.png"))
        (should (s-contains-p "[[file:foo.png]]" md))))))
