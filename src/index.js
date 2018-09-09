import 'purecss/build/pure.css'
import 'animate.css/animate.css'
import './app.styl'
import { Elm } from './Main.elm'

const main = document.createElement('div')
document.body.append(main)
Elm.Main.init({
  node: main
})
