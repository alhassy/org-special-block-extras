;;; org-graph-attributes.el --- The Graphviz attributes that #+begin_graph completes and explains  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; One form: the `org-graph-attributes' database, read by `org-graph-block' for completion, eldoc,
;; and C-u RET.  To add or adjust an attribute, edit its entry here, or `customize-variable' it.

;;; Code:

(defcustom org-graph-attributes
  '((:name "_background" :valid-on (graphs)
     :description "A string in the xdot format specifying an arbitrary background.")
    (:name "area" :valid-on (nodes clusters) :default "1.0"
     :description "Indicates the preferred area for a node or empty cluster. patchwork only.")
    (:name "arrowhead" :valid-on (edges) :default "normal"
     :values ("normal" "inv" "dot" "invdot" "odot" "invodot" "none" "tee" "empty" "invempty" "diamond" "odiamond"
              "ediamond" "crow" "box" "obox" "open" "halfopen" "vee")
     :description "Style of arrowhead on the head node of an edge.")
    (:name "arrowsize" :valid-on (edges) :default "1.0"
     :description "Multiplicative scale factor for arrowheads.")
    (:name "arrowtail" :valid-on (edges) :default "normal"
     :values ("normal" "inv" "dot" "invdot" "odot" "invodot" "none" "tee" "empty" "invempty" "diamond" "odiamond"
              "ediamond" "crow" "box" "obox" "open" "halfopen" "vee")
     :description "Style of arrowhead on the tail node of an edge.")
    (:name "bb" :valid-on (clusters graphs)
     :description "Bounding box of drawing in points. write only.")
    (:name "beautify" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether to draw leaf nodes uniformly in a circle around the root node in sfdp. sfdp only.")
    (:name "bgcolor" :valid-on (graphs clusters)
     :description "Canvas background color.")
    (:name "center" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether to center the drawing in the output canvas.")
    (:name "charset" :valid-on (graphs) :default "\"UTF-8\""
     :description "Character encoding used when interpreting string input as a text label.")
    (:name "class" :valid-on (edges nodes clusters graphs) :default "\"\""
     :description "Classnames to attach to the node, edge, graph, or cluster's SVG element. svg only.")
    (:name "cluster" :valid-on (clusters) :default "false"
     :values ("true" "false")
     :description "Whether the subgraph is a cluster.")
    (:name "clusterrank" :valid-on (graphs) :default "local"
     :values ("local" "global" "none")
     :description "Mode used for handling clusters. dot only.")
    (:name "color" :valid-on (edges nodes clusters) :default "black"
     :description "Basic drawing color for graphics, not text.")
    (:name "colorscheme" :valid-on (edges nodes clusters graphs) :default "\"\""
     :description "A color scheme namespace: the context for interpreting color names.")
    (:name "comment" :valid-on (edges nodes graphs) :default "\"\""
     :description "Comments are inserted into output.")
    (:name "compound" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "If true, allow edges between clusters. dot only.")
    (:name "concentrate" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "If true, use edge concentrators.")
    (:name "constraint" :valid-on (edges) :default "true"
     :values ("true" "false")
     :description "If false, the edge is not used in ranking the nodes. dot only.")
    (:name "Damping" :valid-on (graphs) :default "0.99"
     :description "Factor damping force motions. neato only.")
    (:name "decorate" :valid-on (edges) :default "false"
     :values ("true" "false")
     :description "Whether to connect the edge label to the edge with a line.")
    (:name "defaultdist" :valid-on (graphs) :default "1+(avg. len)*sqrt(|V|)"
     :description "The distance between nodes in separate connected components. neato only.")
    (:name "dim" :valid-on (graphs) :default "2"
     :description "Set the number of dimensions used for the layout. neato, fdp, sfdp only.")
    (:name "dimen" :valid-on (graphs) :default "2"
     :description "Set the number of dimensions used for rendering. neato, fdp, sfdp only.")
    (:name "dir" :valid-on (edges) :default "forward (directed) none (undirected)"
     :values ("forward" "back" "both" "none")
     :description "Edge type for drawing arrowheads.")
    (:name "diredgeconstraints" :valid-on (graphs) :default "false"
     :description "Whether to constrain most edges to point downwards. neato only.")
    (:name "distortion" :valid-on (nodes) :default "0.0"
     :description "Distortion factor for shape=polygon.")
    (:name "dpi" :valid-on (graphs) :default "96.0"
     :description "Specifies the expected number of pixels per inch on a display device. bitmap output, svg only.")
    (:name "edgehref" :valid-on (edges) :default "\"\""
     :description "Synonym for edgeURL. map, svg only.")
    (:name "edgetarget" :valid-on (edges)
     :description "Browser window to use for the edgeURL link. map, svg only.")
    (:name "edgetooltip" :valid-on (edges) :default "\"\""
     :description "Tooltip annotation attached to the non-label part of an edge. cmap, svg only.")
    (:name "edgeURL" :valid-on (edges) :default "\"\""
     :description "The link for the non-label parts of an edge. map, svg only.")
    (:name "epsilon" :valid-on (graphs) :default ".0001 * # nodes (mode == KK) .0001 (mode == major) .01 (mode == sgd)"
     :description "Terminating condition. neato only.")
    (:name "esep" :valid-on (graphs) :default "+3"
     :description "Margin used around polygons for purposes of spline edge routing. neato, fdp, sfdp, osage, circo, twopi only.")
    (:name "fillcolor" :valid-on (nodes edges clusters) :default "lightgrey (nodes) black (clusters)"
     :description "Color used to fill the background of a node or cluster.")
    (:name "fixedsize" :valid-on (nodes) :default "false"
     :values ("true" "false")
     :description "Whether to use the specified width and height attributes to choose node size (rather than sizing to fit the node contents).")
    (:name "fontcolor" :valid-on (edges nodes graphs clusters) :default "black"
     :description "Color used for text.")
    (:name "fontname" :valid-on (edges nodes graphs clusters) :default "\"Times-Roman\""
     :description "Font used for text.")
    (:name "fontnames" :valid-on (graphs) :default "\"\""
     :description "Allows user control of how basic fontnames are represented in SVG output. svg only.")
    (:name "fontpath" :valid-on (graphs) :default "<system-dependent>"
     :description "Directory list used by libgd to search for bitmap fonts.")
    (:name "fontsize" :valid-on (edges nodes graphs clusters) :default "14.0"
     :description "Font size, in points, used for text.")
    (:name "forcelabels" :valid-on (graphs) :default "true"
     :values ("true" "false")
     :description "Whether to force placement of all xlabels, even if overlapping.")
    (:name "gradientangle" :valid-on (nodes clusters graphs) :default "0"
     :description "If a gradient fill is being used, this determines the angle of the fill.")
    (:name "group" :valid-on (nodes) :default "\"\""
     :description "Name for a group of nodes, for bundling edges avoiding crossings. dot only.")
    (:name "head_lp" :valid-on (edges)
     :description "Center position of an edge's head label. write only.")
    (:name "headclip" :valid-on (edges) :default "true"
     :values ("true" "false")
     :description "If true, the head of an edge is clipped to the boundary of the head node.")
    (:name "headhref" :valid-on (edges) :default "\"\""
     :description "Synonym for headURL. map, svg only.")
    (:name "headlabel" :valid-on (edges) :default "\"\""
     :description "Text label to be placed near head of edge.")
    (:name "headport" :valid-on (edges) :default "center"
     :values ("n" "ne" "e" "se" "s" "sw" "w" "nw" "c" "_")
     :description "Indicates where on the head node to attach the head of the edge.")
    (:name "headtarget" :valid-on (edges)
     :description "Browser window to use for the headURL link. map, svg only.")
    (:name "headtooltip" :valid-on (edges) :default "\"\""
     :description "Tooltip annotation attached to the head of an edge. cmap, svg only.")
    (:name "headURL" :valid-on (edges) :default "\"\""
     :description "If defined, headURL is output as part of the head label of the edge. map, svg only.")
    (:name "height" :valid-on (nodes) :default "0.5"
     :description "Height of node, in inches.")
    (:name "href" :valid-on (graphs clusters nodes edges) :default "\"\""
     :description "Synonym for URL. map, postscript, svg only.")
    (:name "id" :valid-on (graphs clusters nodes edges) :default "\"\""
     :description "Identifier for graph objects. map, postscript, svg only.")
    (:name "image" :valid-on (nodes) :default "\"\""
     :description "Gives the name of a file containing an image to be displayed inside a node.")
    (:name "imagepath" :valid-on (graphs) :default "\"\""
     :description "A list of directories in which to look for image files.")
    (:name "imagepos" :valid-on (nodes) :default "\"mc\""
     :description "Controls how an image is positioned within its containing node.")
    (:name "imagescale" :valid-on (nodes) :default "false"
     :values ("true" "false")
     :description "Controls how an image fills its containing node.")
    (:name "inputscale" :valid-on (graphs)
     :description "Scales the input positions to convert between length units. neato, fdp only.")
    (:name "K" :valid-on (graphs clusters) :default "0.3"
     :description "Spring constant used in virtual physical model. fdp, sfdp only.")
    (:name "label" :valid-on (edges nodes graphs clusters) :default "\"\\N\" (nodes) \"\" (otherwise)"
     :description "Text label attached to objects.")
    (:name "label_scheme" :valid-on (graphs) :default "0"
     :description "Whether to treat a node whose name has the form |edgelabel|* as a special node representing an edge label. sfdp only.")
    (:name "labelangle" :valid-on (edges) :default "-25.0"
     :description "The angle (in degrees) in polar coordinates of the head & tail edge labels.")
    (:name "labeldistance" :valid-on (edges) :default "1.0"
     :description "Scaling factor for the distance of headlabel / taillabel from the head / tail nodes.")
    (:name "labelfloat" :valid-on (edges) :default "false"
     :values ("true" "false")
     :description "If true, allows edge labels to be less constrained in position.")
    (:name "labelfontcolor" :valid-on (edges) :default "black"
     :description "Color used for headlabel and taillabel.")
    (:name "labelfontname" :valid-on (edges) :default "\"Times-Roman\""
     :description "Font for headlabel and taillabel.")
    (:name "labelfontsize" :valid-on (edges) :default "14.0"
     :description "Font size of headlabel and taillabel.")
    (:name "labelhref" :valid-on (edges) :default "\"\""
     :description "Synonym for labelURL. map, svg only.")
    (:name "labeljust" :valid-on (graphs clusters) :default "\"c\""
     :values ("l" "r" "c")
     :description "Justification for graph & cluster labels.")
    (:name "labelloc" :valid-on (nodes graphs clusters) :default "\"t\" (clusters) \"b\" (root graphs) \"c\" (nodes)"
     :values ("t" "b" "c")
     :description "Vertical placement of labels for nodes, root graphs and clusters.")
    (:name "labeltarget" :valid-on (edges)
     :description "Browser window to open labelURL links in. map, svg only.")
    (:name "labeltooltip" :valid-on (edges) :default "\"\""
     :description "Tooltip annotation attached to label of an edge. cmap, svg only.")
    (:name "labelURL" :valid-on (edges) :default "\"\""
     :description "If defined, labelURL is the link used for the label of an edge. map, svg only.")
    (:name "landscape" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "If true, the graph is rendered in landscape mode.")
    (:name "layer" :valid-on (edges nodes clusters) :default "\"\""
     :description "Specifies layers in which the node, edge or cluster is present.")
    (:name "layerlistsep" :valid-on (graphs) :default "\",\""
     :description "The separator characters used to split attributes of type layerRange into a list of ranges.")
    (:name "layers" :valid-on (graphs) :default "\"\""
     :description "A linearly ordered list of layer names attached to the graph.")
    (:name "layerselect" :valid-on (graphs) :default "\"\""
     :description "Selects a list of layers to be emitted.")
    (:name "layersep" :valid-on (graphs) :default "\":\\t \""
     :description "The separator characters for splitting the layers attribute into a list of layer names.")
    (:name "layout" :valid-on (graphs) :default "\"\""
     :description "Which layout engine to use.")
    (:name "len" :valid-on (edges) :default "1.0 (neato) 0.3 (fdp)"
     :description "Preferred edge length, in inches. neato, fdp only.")
    (:name "levels" :valid-on (graphs) :default "INT_MAX"
     :description "Number of levels allowed in the multilevel scheme. sfdp only.")
    (:name "levelsgap" :valid-on (graphs) :default "0.0"
     :description "strictness of neato level constraints. neato only.")
    (:name "lhead" :valid-on (edges) :default "\"\""
     :description "Logical head of an edge. dot only.")
    (:name "lheight" :valid-on (graphs clusters)
     :description "Height of graph or cluster label, in inches. write only.")
    (:name "linelength" :valid-on (graphs) :default "128"
     :description "How long strings should get before overflowing to next line, for text output.")
    (:name "lp" :valid-on (edges graphs clusters)
     :description "Label center position. write only.")
    (:name "ltail" :valid-on (edges) :default "\"\""
     :description "Logical tail of an edge. dot only.")
    (:name "lwidth" :valid-on (graphs clusters)
     :description "Width of graph or cluster label, in inches. write only.")
    (:name "margin" :valid-on (nodes clusters graphs) :default "<device-dependent>"
     :description "For graphs, this sets x and y margins of canvas, in inches.")
    (:name "maxiter" :valid-on (graphs) :default "100 * # nodes (mode == KK) 200 (mode == major) 30 (mode == sgd) 600 (fdp)"
     :description "Sets the number of iterations used. neato, fdp only.")
    (:name "mclimit" :valid-on (graphs) :default "1.0"
     :description "Scale factor for mincross (mc) edge crossing minimizer parameters. dot only.")
    (:name "mindist" :valid-on (graphs) :default "1.0"
     :description "Specifies the minimum separation between all nodes. circo only.")
    (:name "minlen" :valid-on (edges) :default "1"
     :description "Minimum edge length (rank difference between head and tail). dot only.")
    (:name "mode" :valid-on (graphs) :default "major"
     :values ("major" "KK" "sgd" "hier" "ipsep" "spring" "maxent")
     :description "Technique for optimizing the layout. neato only.")
    (:name "model" :valid-on (graphs) :default "shortpath"
     :values ("shortpath" "circuit" "subset" "mds")
     :description "Specifies how the distance matrix is computed for the input graph. neato only.")
    (:name "newrank" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether to use a single global ranking, ignoring clusters. dot only.")
    (:name "nodesep" :valid-on (graphs) :default "0.25"
     :description "In dot, nodesep specifies the minimum space between two adjacent nodes in the same rank, in inches.")
    (:name "nojustify" :valid-on (graphs clusters nodes edges) :default "false"
     :values ("true" "false")
     :description "Whether to justify multiline text vs the previous text line (rather than the side of the container).")
    (:name "normalize" :valid-on (graphs) :default "false"
     :description "normalizes coordinates of final layout. neato, fdp, sfdp, twopi, circo only.")
    (:name "notranslate" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether to avoid translating layout to the origin point. neato only.")
    (:name "nslimit" :valid-on (graphs)
     :description "Sets number of iterations in network simplex applications. dot only.")
    (:name "nslimit1" :valid-on (graphs)
     :description "Sets number of iterations in network simplex applications. dot only.")
    (:name "oneblock" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether to draw circo graphs around one circle. circo only.")
    (:name "ordering" :valid-on (graphs nodes) :default "\"\""
     :values ("out" "in")
     :description "Constrains the left-to-right ordering of node edges. dot only.")
    (:name "orientation" :valid-on (nodes graphs) :default "0.0\"\""
     :description "node shape rotation angle, or graph orientation.")
    (:name "outputorder" :valid-on (graphs) :default "breadthfirst"
     :values ("breadthfirst" "nodesfirst" "edgesfirst")
     :description "Specify order in which nodes and edges are drawn.")
    (:name "overlap" :valid-on (graphs) :default "true"
     :values ("true" "false" "scale" "prism" "compress" "vpsc" "ortho")
     :description "Determines if and how node overlaps should be removed. fdp, neato, sfdp, circo, twopi only.")
    (:name "overlap_scaling" :valid-on (graphs) :default "-4"
     :description "Scale layout by factor, to reduce node overlap. prism, neato, sfdp, fdp, circo, twopi only.")
    (:name "overlap_shrink" :valid-on (graphs) :default "true"
     :values ("true" "false")
     :description "Whether the overlap removal algorithm should perform a compression pass to reduce the size of the layout. prism only.")
    (:name "pack" :valid-on (graphs) :default "false"
     :values ("true" "false")
     :description "Whether each connected component of the graph should be laid out separately, and then the graphs packed together.")
    (:name "packmode" :valid-on (graphs) :default "node"
     :values ("node" "clust" "graph")
     :description "How connected components should be packed.")
    (:name "pad" :valid-on (graphs) :default "0.0555 (4 points)"
     :description "Inches to extend the drawing area around the minimal area needed to draw the graph.")
    (:name "page" :valid-on (graphs)
     :description "Width and height of output pages, in inches.")
    (:name "pagedir" :valid-on (graphs) :default "BL"
     :values ("BL" "BR" "TL" "TR" "RB" "RT" "LB" "LT")
     :description "The order in which pages are emitted.")
    (:name "pencolor" :valid-on (clusters) :default "black"
     :description "Color used to draw the bounding box around a cluster.")
    (:name "penwidth" :valid-on (clusters nodes edges) :default "1.0"
     :description "Specifies the width of the pen, in points, used to draw lines and curves.")
    (:name "peripheries" :valid-on (nodes clusters) :default "<shape default> (nodes) 1 (clusters)"
     :description "Set number of peripheries used in polygonal shapes and cluster boundaries.")
    (:name "pin" :valid-on (nodes) :default "false"
     :values ("true" "false")
     :description "Keeps the node at the node's given input position. neato, fdp only.")
    (:name "pos" :valid-on (edges nodes)
     :description "Position of node, or spline control points. neato, fdp only.")
    (:name "quadtree" :valid-on (graphs) :default "normal"
     :values ("normal" "fast" "none")
     :description "Quadtree scheme to use. sfdp only.")
    (:name "quantum" :valid-on (graphs) :default "0.0"
     :description "If quantum > 0.0, node label dimensions will be rounded to integral multiples of the quantum.")
    (:name "radius" :valid-on (edges) :default "0"
     :description "Radius of rounded corners on orthogonal edges.")
    (:name "rank" :valid-on (clusters)
     :values ("same" "min" "max" "source" "sink")
     :description "Rank constraints on the nodes in a subgraph. dot only.")
    (:name "rankdir" :valid-on (graphs) :default "TB"
     :values ("TB" "LR" "BT" "RL")
     :description "Sets direction of graph layout. dot only.")
    (:name "ranksep" :valid-on (graphs) :default "0.5 (dot) 1.0 (twopi)"
     :description "Specifies separation between ranks. dot, twopi only.")
    (:name "ratio" :valid-on (graphs)
     :description "Sets the aspect ratio (drawing height/drawing width) for the drawing.")
    (:name "rects" :valid-on (nodes)
     :description "Rectangles for fields of records, in points. write only.")
    (:name "regular" :valid-on (nodes) :default "false"
     :values ("true" "false")
     :description "If true, force polygon to be regular.")
    (:name "remincross" :valid-on (graphs) :default "true"
     :values ("true" "false")
     :description "If there are multiple clusters, whether to run edge crossing minimization a second time. dot only.")
    (:name "repulsiveforce" :valid-on (graphs) :default "1.0"
     :description "The power of the repulsive force used in an extended Fruchterman-Reingold. sfdp only.")
    (:name "resolution" :valid-on (graphs) :default "96.0"
     :description "Synonym for dpi. bitmap output, svg only.")
    (:name "root" :valid-on (graphs nodes) :default "<none> (graphs) false (nodes)"
     :description "Specifies nodes to be used as the center of the layout. twopi, circo only.")
    (:name "rotate" :valid-on (graphs) :default "0"
     :description "If rotate=90, sets drawing orientation to landscape.")
    (:name "rotation" :valid-on (graphs) :default "0"
     :description "Rotates the final layout counter-clockwise by the specified number of degrees. sfdp only.")
    (:name "samehead" :valid-on (edges) :default "\"\""
     :description "Edges with the same head and the same samehead value are aimed at the same point on the head. dot only.")
    (:name "sametail" :valid-on (edges) :default "\"\""
     :description "Edges with the same tail and the same sametail value are aimed at the same point on the tail. dot only.")
    (:name "samplepoints" :valid-on (nodes) :default "8 (output) 20 (overlap and image maps)"
     :description "Gives the number of points used for a circle/ellipse node.")
    (:name "scale" :valid-on (graphs)
     :description "Scales layout by the given factor after the initial layout. neato, twopi only.")
    (:name "searchsize" :valid-on (graphs) :default "30"
     :description "During network simplex, the maximum number of edges with negative cut values to search when looking for an edge with minimum cut value. dot only.")
    (:name "sep" :valid-on (graphs) :default "+4"
     :description "Margin to leave around nodes when removing node overlap. fdp, neato, sfdp, osage, circo, twopi only.")
    (:name "shape" :valid-on (nodes) :default "ellipse"
     :values ("box" "polygon" "ellipse" "oval" "circle" "point" "egg" "triangle" "plaintext" "plain" "diamond"
              "trapezium" "parallelogram" "house" "pentagon" "hexagon" "septagon" "octagon" "doublecircle"
              "doubleoctagon" "tripleoctagon" "invtriangle" "invtrapezium" "invhouse" "Mdiamond" "Msquare" "Mcircle"
              "rect" "rectangle" "square" "star" "none" "underline" "cylinder" "note" "tab" "folder" "box3d"
              "component" "promoter" "cds" "terminator" "utr" "primersite" "restrictionsite" "fivepoverhang"
              "threepoverhang" "noverhang" "assembly" "signature" "insulator" "ribosite" "rnastab" "proteasesite"
              "proteinstab" "rpromoter" "rarrow" "larrow" "lpromoter" "record" "Mrecord")
     :description "Sets the shape of a node.")
    (:name "shapefile" :valid-on (nodes) :default "\"\""
     :description "A file containing user-supplied node content.")
    (:name "showboxes" :valid-on (edges nodes graphs) :default "0"
     :description "Print guide boxes for debugging. dot only.")
    (:name "sides" :valid-on (nodes) :default "4"
     :description "Number of sides when shape=polygon.")
    (:name "size" :valid-on (graphs)
     :description "Maximum width and height of drawing, in inches.")
    (:name "skew" :valid-on (nodes) :default "0.0"
     :description "Skew factor for shape=polygon.")
    (:name "smoothing" :valid-on (graphs) :default "\"none\""
     :values ("none" "avg_dist" "graph_dist" "power_dist" "rng" "spring" "triangle")
     :description "Specifies a post-processing step used to smooth out an uneven distribution of nodes. sfdp only.")
    (:name "sortv" :valid-on (graphs clusters nodes) :default "0"
     :description "Sort order of graph components for ordering packmode packing.")
    (:name "splines" :valid-on (graphs)
     :values ("spline" "line" "polyline" "curved" "ortho" "none")
     :description "Controls how, and if, edges are represented.")
    (:name "start" :valid-on (graphs) :default "\"\""
     :values ("regular" "self" "random")
     :description "Parameter used to determine the initial layout of nodes. neato, fdp, sfdp only.")
    (:name "style" :valid-on (edges nodes clusters graphs) :default "\"\""
     :values ("filled" "rounded" "filled,rounded" "dashed" "dotted" "solid" "bold" "invis" "striped" "wedged"
              "diagonals" "radial" "tapered")
     :description "Set style information for components of the graph.")
    (:name "stylesheet" :valid-on (graphs) :default "\"\""
     :description "A URL or pathname specifying an XML style sheet, used in SVG output. svg only.")
    (:name "tail_lp" :valid-on (edges)
     :description "Position of an edge's tail label, in points. write only.")
    (:name "tailclip" :valid-on (edges) :default "true"
     :values ("true" "false")
     :description "If true, the tail of an edge is clipped to the boundary of the tail node.")
    (:name "tailhref" :valid-on (edges) :default "\"\""
     :description "Synonym for tailURL. map, svg only.")
    (:name "taillabel" :valid-on (edges) :default "\"\""
     :description "Text label to be placed near tail of edge.")
    (:name "tailport" :valid-on (edges) :default "center"
     :values ("n" "ne" "e" "se" "s" "sw" "w" "nw" "c" "_")
     :description "Indicates where on the tail node to attach the tail of the edge.")
    (:name "tailtarget" :valid-on (edges)
     :description "Browser window to use for the tailURL link. map, svg only.")
    (:name "tailtooltip" :valid-on (edges) :default "\"\""
     :description "Tooltip annotation attached to the tail of an edge. cmap, svg only.")
    (:name "tailURL" :valid-on (edges) :default "\"\""
     :description "If defined, tailURL is output as part of the tail label of the edge. map, svg only.")
    (:name "target" :valid-on (edges nodes graphs clusters)
     :description "If the object has a URL, this attribute determines which window of the browser is used for the URL. map, svg only.")
    (:name "TBbalance" :valid-on (graphs) :default "''"
     :description "Which rank to move floating (loose) nodes to. dot only.")
    (:name "tooltip" :valid-on (nodes edges clusters graphs) :default "\"\""
     :description "Tooltip (mouse hover text) attached to the node, edge, cluster, or graph. cmap, svg only.")
    (:name "truecolor" :valid-on (graphs)
     :values ("true" "false")
     :description "Whether internal bitmap rendering relies on a truecolor color model or uses. bitmap output only.")
    (:name "URL" :valid-on (edges nodes graphs clusters)
     :description "Hyperlinks incorporated into device-dependent output. map, postscript, svg only.")
    (:name "vertices" :valid-on (nodes)
     :description "Sets the coordinates of the vertices of the node's polygon, in inches. write only.")
    (:name "viewport" :valid-on (graphs) :default "\"\""
     :description "Clipping window on final drawing.")
    (:name "voro_margin" :valid-on (graphs) :default "0.05"
     :description "Tuning margin of Voronoi technique. neato, fdp, sfdp, twopi, circo only.")
    (:name "weight" :valid-on (edges) :default "1"
     :description "Weight of edge.")
    (:name "width" :valid-on (nodes) :default "0.75"
     :description "Width of node, in inches.")
    (:name "xdotversion" :valid-on (graphs)
     :description "Determines the version of xdot used in output. xdot only.")
    (:name "xlabel" :valid-on (edges nodes) :default "\"\""
     :description "External label for a node or edge.")
    (:name "xlp" :valid-on (nodes edges)
     :description "Position of an exterior label, in points. write only.")
    (:name "z" :valid-on (nodes) :default "0.0"
     :description "Z-coordinate value for 3D layouts and displays."))
  "Every Graphviz attribute, for completion, eldoc, and `org-graph--insert-attribute-row'.

Scraped from the table at https://graphviz.org/doc/info/attrs.html on 2026-10-14; :values are the enumerations
of each attribute's type, per https://graphviz.org/docs/attr-types/.  Each entry is a plist with:
  :name        ↦ attribute name string
  :description ↦ one-line human description, ending with the layouts it is limited to, if any
  :values      ↦ list of completion candidates, or nil for free-text entry
  :default     ↦ Graphviz's default, as its docs state it; nil when it has none
  :valid-on    ↦ context symbols: any subset of (nodes edges graphs clusters); subgraph attributes count as clusters."
  :type '(repeat
          (plist :options
                 ((:name        string)
                  (:description string)
                  (:values      (choice (const nil) (repeat string)))
                  (:default     (choice (const nil) string))
                  (:valid-on    (repeat (choice (const nodes)
                                                (const edges)
                                                (const graphs)
                                                (const clusters)))))))
  :group 'org-special-block-extras)

(provide 'org-graph-attributes)
;;; org-graph-attributes.el ends here
