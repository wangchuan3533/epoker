var path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: './index.jsx',
  output: { path: __dirname, filename: 'bundle.js' },
  resolve: {
    extensions: [ '', '.js', '.jsx', '.css', '.scss', '.json']
  },
  module: {
    loaders: [
      {
        test: /.jsx?$/,
        loader: 'babel-loader',
        exclude: /node_modules/,
        query: {
          presets: ['es2015', 'stage-0', 'react']
        }
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
