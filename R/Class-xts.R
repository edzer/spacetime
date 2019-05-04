setOldClass("xts")
setOldClass("zoo")
tzone = function(x, ...) {
  if (utils::packageVersion("xts") <= "0.11.2") {
    if (inherits(x, "xts")) {
      tzoneAttr = attr(xts::.index(x), "tzone")
    } else {
      tzoneAttr = attr(x, "tzone")
    }
  } else {
    tzoneAttr = xts::tzone(x, ...)
  }
  tzoneAttr
}
