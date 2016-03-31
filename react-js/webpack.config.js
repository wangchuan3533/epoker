var path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: './index.js',
  output: { path: __dirname, filename: 'bundle.js' },
  resolve: {
    extensions: [ '', '.js', '.jsx', '.css', '.scss', '.json']
  },
  module: {
    loaders: [
      {
        test: /.js?$/,
        loader: 'babel',
        exclude: /node_modules/
      },
      {
        test: /\.css$/,
        loader: "style-loader!css-loader"
      },
      {
        test: /\.scss$/,
        loaders: ["style", "css", "sass"]
      }
    ]
  },
};
