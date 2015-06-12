gulp = require("gulp")
uglify = require("gulp-uglify")
watch = require("gulp-watch")
webpack = require("gulp-webpack")
_ = require("lodash")

webpackConfig =
  output:
    filename: "script.js"
  resolveLoader:
    modulesDirectories: ["node_modules"]
  resolve:
    modulesDirectories: ["node_modules", ".purescript"]
    extensions: ["", ".js", ".coffee", ".cjsx", ".jsx", ".purs"]
  module:
    loaders: [
      # { test: /\.jsx$/, loader: "jsx?harmony" },
      { test: /\.cjsx$/, loaders: ["coffee?harmony", "cjsx?harmony"]}
      { test: /\.purs$/, loader: "purs-loader?src[]=src&output=.purescript"}
    ]


webpackWatchConfig = _.merge _.clone(webpackConfig), { watch: true }

gulp.task "default", ->
  gulp.src("./src/main.cjsx")
    .pipe(webpack(webpackConfig))
    # .pipe(uglify())
    .pipe(gulp.dest("./build"))

  gulp.src("./bower_components/fetch/fetch.js")
    .pipe(uglify())
    .pipe(gulp.dest("./build"))

gulp.task "watch", ->
  gulp.src("./src/main.cjsx")
    .pipe(webpack(webpackWatchConfig))
    .pipe(gulp.dest("./build"))
