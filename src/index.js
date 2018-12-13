import '../node_modules/purecss/build/pure.css'
import '../node_modules/animate.css/animate.css'
import './app.styl'
import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main,
  flags: {
    apiRoot: process.env.API_ROOT ? process.env.API_ROOT: 'http://localhost:8000'
  }
})
