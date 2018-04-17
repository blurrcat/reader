import 'purecss/build/pure.css'
import './base.css'
import Elm from './Main.elm'


const main = document.createElement('div')
Elm.Main.embed(main)
document.body.append(main)
