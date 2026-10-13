;;; org-graph-html-export-tests.el --- HTML export  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; HTML export

(deftest "`#+begin_graph' HTML export produces an <img> tag"
  ;; Call the graph block handler directly: simulate what org-export-string
  ;; does for a #+begin_graph block, bypassing the full export pipeline
  ;; (which triggers org-defblock docstring enrichment and breaks in batch).
  (cl-letf (((symbol-function 'org-graph--render-to-png)
             (lambda (_dot pngfile)
               ;; Write a minimal 1×1 white PNG so base64 encoding succeeds.
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "\x89PNG\r\n\x1a\n"))
               t)))
    (let ((html (org-block/graph 'html "+ A\n  + B\n    | EDGE | label=calls |\n"
                                 "directed" nil)))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' HTML export: node and edge appear in underlying dot"
  ;; Verify the dot source that gets embedded is structurally correct
  ;; by checking the raw->dot output directly.
  (let ((dot (org--graph-raw->dot
              "+ A\n  + B\n    | EDGE | label=calls |\n"
              "directed")))
    (should (s-contains-p "A -> B [ label=calls ];" dot))
    (should (s-contains-p "digraph" dot))))

(deftest "`#+begin_graph' :svg t — HTML export embeds inline <svg>"
  ;; With :svg t the handler calls org-graph--render-to-svg and returns
  ;; the raw <svg>…</svg> string for inline embedding (no base64, no <img>).
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot svgfile)
               (with-temp-file svgfile
                 (insert "<?xml version=\"1.0\"?>\n<!DOCTYPE svg>\n<svg xmlns=\"http://www.w3.org/2000/svg\"><g><title>A</title></g></svg>\n"))
               svgfile)))
    (let ((html (org-block/graph 'html "+ A\n" "directed" :svg t)))
      (should (s-contains-p "<svg" html))
      (should (s-contains-p "<title>A</title>" html))
      (should-not (s-contains-p "<img" html))
      (should-not (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' :svg t — XML preamble is stripped"
  ;; org-graph--svg-contents must strip the <?xml …?> and <!DOCTYPE> lines
  ;; so only the <svg>…</svg> element is embedded in the HTML.
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot svgfile)
               (with-temp-file svgfile
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"\">\n<svg xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>\n"))
               svgfile)))
    (let ((html (org-block/graph 'html "+ A\n" "directed" :svg t)))
      (should-not (s-contains-p "<?xml" html))
      (should-not (s-contains-p "<!DOCTYPE" html))
      (should (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' :svg t — falls back to <pre> when dot unavailable"
  ;; If org-graph--render-to-svg returns nil (dot not installed), the
  ;; handler must fall back to a <pre> block with escaped dot source.
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot _svgfile) nil)))
    (let ((html (org-block/graph 'html "+ A\n  + B\n" "directed" :svg t)))
      (should (s-contains-p "<pre>" html))
      (should-not (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' no :svg — HTML export still produces base64 PNG"
  ;; Omitting :svg (svg=nil) must leave the existing PNG path untouched.
  (cl-letf (((symbol-function 'org-graph--render-to-png)
             (lambda (_dot pngfile)
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "\x89PNG\r\n\x1a\n"))
               t)))
    (let ((html (org-block/graph 'html "+ A\n" "directed")))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html))
      (should-not (s-contains-p "<svg" html)))))
