const path = require('path')
const env = process.env.NODE_ENV ? process.env.NODE_ENV : 'development'
const HtmlWebpackPlugin = require('html-webpack-plugin')

const debug = env === 'development'
console.log('building in ' + env + ' environment..')

module.exports = {
  mode: env,
  entry: {
    app: ['./src/index.js']
  },

  plugins: [
    new HtmlWebpackPlugin({
      title: 'Feed Reader',
      meta: {
        viewport: 'width=device-width, initial-scale=1'
      },
      hash: true
    })
  ],

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].[chunkhash].js'
  },

  module: {
    rules: [
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\.styl(us)?$/,
        use: [
          'style-loader',
          'css-loader',
          {
            loader: 'postcss-loader',
            options: {
              plugins: () => [require('autoprefixer')()]
            }
          },
          'stylus-loader'
        ]
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?verbose=true&warn=true',
        options: {
          debug: debug,
          optimize: !debug
        }
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff'
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader'
      }
    ],

    noParse: /\.elm$/
  },

  devServer: {
    inline: true,
    compress: true,
    stats: { colors: true }
  }
}
