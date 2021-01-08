library("R6")
library("xml2")

Localization <- R6Class("Localization",
  public = list(
    filename=NULL,
    tree = NULL,
    root_node=NULL,
    initialize = function(filename="localization.xml", language=NULL) {
      self$filename <- filename
      self$tree <- read_xml("localization.xml")
      self$root_node <- self$tree # Change for language settings
      if (!is.null(language)) {
        self$set_root_node(language)
      }
    },
    set_root_node = function(xpath) {
      nodes = self$get_nodes(xpath)
      if (length(nodes) == 1) {
        self$root_node=nodes[1]
        return(self)
      } else if (length(nodes) == 0) {
        stop("no root node found")
      } else {
        stop("multiple root nodes found")
      }
      
    },
    get_nodes = function(xpath) {
      # returns the nodes along a xpath
      nodes <- xml_find_all(self$root_node, xpath)
      return(nodes)
    },
    get_list_attr_from_nodes = function(nodes, attr) {
      # returns a list of the attribute values for nodes
      alist <- sapply(nodes, function(node){xml_attr(node, attr)})
      return(alist)
    },
    get_list_attr_from_xpath = function(xpath, attr) {
      # returns a list of the attribute values for a xpath
      nodes <- xml_find_all(self$root_node, xpath)
      alist <- self$get_list_attr_from_nodes(nodes, attr)
      return(alist)
    },
    get_map_attr2attr_from_nodes = function(nodes, key_attr, value_attr) {
      # returns a mapping from key_attr to value_attr along nodes
      keys <- sapply(nodes, function(node){xml_attr(node, key_attr)})
      values <- sapply(nodes, function(node){xml_attr(node, value_attr)})
      mapping <- as.list(setNames(values, keys))
      return(mapping)
    },
    get_map_attr2attr_from_xpath = function(xpath, key_attr, value_attr) {
      # returns a mapping from key_attr to value_attr along a xpath
      nodes <- self$get_nodes(xpath)
      mapping <- self$get_map_attr2attr_from_nodes(nodes, key_attr, value_attr)
      return(mapping)
    },
    get_map_attr2text_from_nodes = function(nodes, key_attr) {
      # returns a mapping from key_attr to text along a node
      keys <- sapply(nodes, function(node){xml_attr(node, key_attr)})
      values <- sapply(nodes, xml_text)
      mapping <- as.list(setNames(values, keys))
      return(mapping)
    },
    get_map_attr2text_from_xpath = function(xpath, key_attr) {
      # returns a mapping from key_attr to text along nodes
      nodes <- self$get_nodes(xpath)
      mapping <- self$get_map_attr2text_from_nodes(nodes, key_attr)
      return(mapping)
    }
  ) # public
)

