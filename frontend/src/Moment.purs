module Moment where

foreign import moment """
  function moment(src) {
    return function(format) {
      return require("moment")(src).format(format);
    };
  }
  """ :: String -> String -> String
