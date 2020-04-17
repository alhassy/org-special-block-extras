<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<!-- 2020-04-16 Thu 22:37 -->
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>org-special-block-extras</title>
<meta name="generator" content="Org mode" />
<meta name="author" content="Musa Al-hassy" />
<style type="text/css">
 <!--/*--><![CDATA[/*><!--*/
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #ccc;
    box-shadow: 3px 3px 3px #eee;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: visible;
    padding-top: 1.2em;
  }
  pre.src:before {
    display: none;
    position: absolute;
    background-color: white;
    top: -10px;
    right: 10px;
    padding: 3px;
    border: 1px solid black;
  }
  pre.src:hover:before { display: inline;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-hledger:before { content: 'hledger'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'Javascript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per "defun org-babel-execute"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf " ")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .equation-container {
    display: table;
    text-align: center;
    width: 100%;
  }
  .equation {
    vertical-align: middle;
  }
  .equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
  }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { width: 90%; }
  /*]]>*/-->
</style>
<script type="text/javascript">
/*
@licstart  The following is the entire license notice for the
JavaScript code in this tag.

Copyright (C) 2012-2020 Free Software Foundation, Inc.

The JavaScript code in this tag is free software: you can
redistribute it and/or modify it under the terms of the GNU
General Public License (GNU GPL) as published by the Free Software
Foundation, either version 3 of the License, or (at your option)
any later version.  The code is distributed WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

As additional permission under GNU GPL version 3 section 7, you
may distribute non-source (e.g., minimized or compacted) forms of
that code without the copy of the GNU GPL normally required by
section 4, provided you include this license notice and a URL
through which recipients can access the Corresponding Source.


@licend  The above is the entire license notice
for the JavaScript code in this tag.
*/
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = "code-highlighted";
     elem.className   = "code-highlighted";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>
</head>
<body>
<div id="content">
<h1 class="title">org-special-block-extras</h1>
<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#Example-Use">1. Example Use</a></li>
<li><a href="#Core-Utility">2. Core Utility</a></li>
<li><a href="#org3c28301">3. Colours</a></li>
<li><a href="#Parallel">4. Parallel</a></li>
</ul>
</div>
</div>

<div id="outline-container-org4bbcd04" class="outline-2">
<h2 id="Example-Use"><span class="section-number-2">1</span> Example Use</h2>
<div class="outline-text-2" id="text-Example-Use">
<p>
User type something along the lines of the following.
</p>
<pre class="example">
#+begin_red org
/This/
      *text*
             _is_
                  red!
#+end_red
</pre>

<p>
Which generates <i>red</i> text when exported to HTML and LaTeX,
<b>while supporting Org markup</b>.
</p>

<blockquote>
<p>
This article may be read as a <a href="https://alhassy.github.io/org-special-block-extras/README.pdf">PDF</a> or as <a href="https://alhassy.github.io/org-special-block-extras/README.html">HTML</a> or as pure <a href="https://alhassy.github.io/org-special-block-extras/README.org">Org</a>!
</p>
</blockquote>


<div class="figure">
<p><img src="images/colours.jpg" alt="colours.jpg" />
</p>
<p><span class="figure-number">Figure 1: </span>Write Org-markup once, generate for many backends ^_^</p>
</div>

<div style="color:black;"><p>
This text is black!
</p>
</div>

<div style="color:blue;"><p>
This text is blue!
</p>
</div>

<div style="color:brown;"><p>
This text is brown!
</p>
</div>

<div style="color:cyan;"><p>
This text is cyan!
</p>
</div>

<div style="color:darkgray;"><p>
This text is darkgray!
</p>
</div>

<div style="color:gray;"><p>
This text is gray!
</p>
</div>

<div style="color:green;"><p>
This text is green!
</p>
</div>

<div style="color:lightgray;"><p>
This text is lightgray!
</p>
</div>

<div style="color:lime;"><p>
This text is lime!
</p>
</div>

<div style="color:magenta;"><p>
This text is magenta!
</p>
</div>

<div style="color:olive;"><p>
This text is olive!
</p>
</div>

<div style="color:orange;"><p>
This text is orange!
</p>
</div>

<div style="color:pink;"><p>
This text is pink!
</p>
</div>

<div style="color:purple;"><p>
This text is purple!
</p>
</div>

<div style="color:red;"><p>
This text is red!
</p>
</div>

<div style="color:teal;"><p>
This text is teal!
</p>
</div>

<div style="color:violet;"><p>
This text is violet!
</p>
</div>

<div style="color:white;"><p>
This text is white!
</p>
</div>

<div style="color:yellow;"><p>
This text is yellow!
</p>
</div>

<p>
The remaining sections are implementation matter.
</p>
</div>
</div>
<div id="outline-container-orgb2e427b" class="outline-2">
<h2 id="Core-Utility"><span class="section-number-2">2</span> Core Utility</h2>
<div class="outline-text-2" id="text-Core-Utility">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #96A7A9; font-style: italic;">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">Core utility</span>

(<span style="color: #859900; font-weight: bold;">defun</span> <span style="color: #b58900;">org-special-block-extras--advice</span> (backend blk contents _)
  <span style="color: #35a69c; font-style: italic;">"Invoke the appropriate custom block handler, if any.</span>

<span style="color: #35a69c; font-style: italic;">A given custom block BLK has a TYPE extracted from it, then we</span>
<span style="color: #35a69c; font-style: italic;">send the block CONTENTS along with the current export BACKEND to</span>
<span style="color: #35a69c; font-style: italic;">the formatting function ORG-SPECIAL-BLOCK-EXTRAS/TYPE if it is</span>
<span style="color: #35a69c; font-style: italic;">defined, otherwise, we leave the CONTENTS of the block as is."</span>
  (<span style="color: #859900; font-weight: bold;">let*</span> ((type    (<span style="color: #b58900;">nth</span> 1 (<span style="color: #b58900;">nth</span> 1 blk)))
         (handler (<span style="color: #b58900;">intern</span> (<span style="color: #b58900;">format</span> <span style="color: #2aa198;">"org-special-block-extras--%s"</span> type))))
    (<span style="color: #859900; font-weight: bold;">ignore-errors</span> (<span style="color: #b58900;">apply</span> handler backend contents nil))))

(<span style="color: #b58900;">advice-add</span> #'<span style="color: #b58900;">org-html-special-block</span> <span style="color: #d33682; font-style: italic;">:before-until</span>
            (<span style="color: #b58900;">-partial</span> #'<span style="color: #b58900;">org-special-block-extras--advice</span> 'html))

(<span style="color: #b58900;">advice-add</span> #'<span style="color: #b58900;">org-latex-special-block</span> <span style="color: #d33682; font-style: italic;">:before-until</span>
            (<span style="color: #b58900;">-partial</span> #'<span style="color: #b58900;">org-special-block-extras--advice</span> 'latex))
</pre>
</div>
</div>
</div>

<div id="outline-container-org3c28301" class="outline-2">
<h2 id="org3c28301"><span class="section-number-2">3</span> Colours</h2>
<div class="outline-text-2" id="text-3">
<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #96A7A9; font-style: italic;">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<span style="color: #96A7A9; font-style: italic;">;;</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">Load support for 19 colour custom blocks</span>

(<span style="color: #859900; font-weight: bold;">defvar</span> <span style="color: #268bd2;">org-special-block-extras--colors</span>
  '(black blue brown cyan darkgray gray green lightgray lime
          magenta olive orange pink purple red teal violet white
          yellow)
  <span style="color: #35a69c; font-style: italic;">"Colours that should be available on all systems."</span>)

(<span style="color: #859900; font-weight: bold;">loop</span> for colour in <span style="color: #268bd2;">org-special-block-extras--colors</span>
      <span style="color: #b58900;">do</span> (<span style="color: #b58900;">eval</span> (<span style="color: #b58900;">read</span> (<span style="color: #b58900;">format</span>
                      <span style="color: #2aa198;">"(defun org-special-block-extras--%s (backend contents)</span>
<span style="color: #2aa198;">                     (format (pcase backend</span>
<span style="color: #2aa198;">                     (`latex \"\\\\begingroup\\\\color{%s}%%s\\\\endgroup\")</span>
<span style="color: #2aa198;">                     (`html  \"&lt;div style=\\\"color:%s;\\\"&gt;%%s&lt;/div&gt;\")</span>
<span style="color: #2aa198;">                     (t      \"org-special-block-extras: Unsupported backend\"))</span>
<span style="color: #2aa198;">                     contents))"</span>
                      colour colour colour))))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgd9ce88f" class="outline-2">
<h2 id="Parallel"><span class="section-number-2">4</span> Parallel</h2>
<div class="outline-text-2" id="text-Parallel">
<div style="column-rule-style:solid;column-count:2;"<p>
<span class="underline">Example:</span>
</p>
<pre class="example">
#+begin_3parallel org
one

#+latex: \columnbreak
two

#+latex: \columnbreak
three
#+end_3parallel
</pre>

<p>
<span class="underline">Yields:</span>
</p>
<div style="column-rule-style:solid;column-count:3;"<p>
one
</p>

<p>
two
</p>

<p>
three
</p>
</div>
</div>

<p>
I initially used the names <code>paralell&lt;n&gt;</code> but names ending with a number did not
inherit highlighting, so I shifted the number to being a prefix instead.
</p>
<ul class="org-ul">
<li>For LaTeX, new lines are used to suggest opportunities for column breaks
and are needed even if explicit columnbreaks are declared.</li>
</ul>

<div class="org-src-container">
<pre class="src src-emacs-lisp"><span style="color: #96A7A9; font-style: italic;">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<span style="color: #96A7A9; font-style: italic;">;;</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">Parallel blocks: parallel&lt;n&gt;[NB] for n:2..5, optionally with &#8216;N&#8217;o &#8216;b&#8217;ar</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">in-between the columns.</span>
<span style="color: #96A7A9; font-style: italic;">;;</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">Common case is to have three columns, and we want to avoid invoking the</span>
<span style="color: #96A7A9; font-style: italic;">;; </span><span style="color: #96A7A9; font-style: italic;">attribute via org, so making this.</span>

(<span style="color: #859900; font-weight: bold;">loop</span> for cols in '(<span style="color: #2aa198;">"1"</span> <span style="color: #2aa198;">"2"</span> <span style="color: #2aa198;">"3"</span> <span style="color: #2aa198;">"4"</span> <span style="color: #2aa198;">"5"</span>)
      <span style="color: #b58900;">do</span> (<span style="color: #859900; font-weight: bold;">loop</span> for rule in '(<span style="color: #2aa198;">"solid"</span> <span style="color: #2aa198;">"none"</span>)
      <span style="color: #b58900;">do</span> (<span style="color: #b58900;">eval</span> (<span style="color: #b58900;">read</span> (<span style="color: #b58900;">concat</span>
<span style="color: #2aa198;">"(defun org-special-block-extras--"</span> cols <span style="color: #2aa198;">"parallel"</span>
(<span style="color: #859900; font-weight: bold;">if</span> (<span style="color: #b58900;">equal</span> rule <span style="color: #2aa198;">"solid"</span>) <span style="color: #2aa198;">""</span> <span style="color: #2aa198;">"NB"</span>)
<span style="color: #2aa198;">"(backend contents)"</span>
<span style="color: #2aa198;">"(format (pcase backend"</span>
<span style="color: #2aa198;">"(`html \"&lt;div style=\\\"column-rule-style:"</span> rule <span style="color: #2aa198;">";column-count:"</span> cols <span style="color: #2aa198;">";\\\"%s&lt;/div&gt;\")"</span>
<span style="color: #2aa198;">"(`latex \"\\\\par \\\\setlength{\\\\columnseprule}{"</span> (<span style="color: #859900; font-weight: bold;">if</span> (<span style="color: #b58900;">equal</span> rule <span style="color: #2aa198;">"solid"</span>) <span style="color: #2aa198;">"2"</span> <span style="color: #2aa198;">"0"</span>) <span style="color: #2aa198;">"pt}"</span>
<span style="color: #2aa198;">"          \\\\begin{minipage}[t]{\\\\linewidth}"</span>
<span style="color: #2aa198;">"          \\\\begin{multicols}{"</span> cols <span style="color: #2aa198;">"}"</span>
<span style="color: #2aa198;">"          %s"</span>
<span style="color: #2aa198;">"          \\\\end{multicols}\\\\end{minipage}\")) contents))"</span>)))))

(<span style="color: #859900; font-weight: bold;">defalias</span> #'<span style="color: #b58900;">org-special-block-extras--parallel</span>   #'<span style="color: #b58900;">org-special-block-extras--2parallel</span>)
(<span style="color: #859900; font-weight: bold;">defalias</span> #'<span style="color: #b58900;">org-special-block-extras--parallelNB</span> #'<span style="color: #b58900;">org-special-block-extras--2parallelNB</span>)
</pre>
</div>
</div>
</div>
</div>
<div id="postamble" class="status">
<p class="author">Author: Musa Al-hassy</p>
<p class="date">Created: 2020-04-16 Thu 22:37</p>
<p class="validation"><a href="http://validator.w3.org/check?uri=referer">Validate</a></p>
</div>
</body>
</html>
