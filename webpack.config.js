var path = require('path');
var webpack = require('webpack');
var PurescriptWebpackPlugin = require('purescript-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: [ path.join(__dirname, 'src/js/index.js') ],
  debug: true,
  devtool: 'cheap-module-eval-source-map',
  output: {
    path: path.join(__dirname, '/dist/'),
    filename: '[name].js',
    publicPath: '/'
  },
  module: {
    loaders: [
      { test: /\.js$/, loader: 'source-map-loader', exclude: /node_modules|bower_components/ },
      { test: /\.purs$/, loader: 'purs-loader', exclude: /node_modules/ }
    ],
  },
  plugins: [
    new PurescriptWebpackPlugin({
      src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
      ffi: ['bower_components/purescript-*/src/**/*.js'],
      bundle: false,
      psc: 'psa',
      pscArgs: {
        sourceMaps: true
      }
    }),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
    new webpack.optimize.OccurenceOrderPlugin(true),
    new HtmlWebpackPlugin({
      template: 'src/html/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new webpack.SourceMapDevToolPlugin({
      filename: '[file].map',
      moduleFilenameTemplate: '[absolute-resource-path]',
      fallbackModuleFilenameTemplate: '[absolute-resource-path]'
    }),
    new webpack.NoErrorsPlugin()
  ],
  resolveLoader: {
    root: path.join(__dirname, 'node_modules')
  },
  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['', '.js', '.purs']
  },
  devServer: {
    port: 3000,
    host: 'localhost',
    historyApiFallback: true,
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    }
  }
};
