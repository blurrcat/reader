import '../node_modules/purecss/build/pure.css'
import '../node_modules/animate.css/animate.css'
import './main.styl'
import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main,
  flags: {}
})
