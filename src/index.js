import '../node_modules/purecss/build/pure.css'
import '../node_modules/animate.css/animate.css'
import '../node_modules/colors.css/css/colors.css'
import './main.styl'
import { Elm } from './Main.elm'

const storageKey = 'store'
const localStorage = window.localStorage
const flags = localStorage.getItem(storageKey)

const app = Elm.Main.init({
  flags: flags
})

app.ports.storeCache.subscribe((val) => {
  if (val) {
    localStorage.setItem(storageKey, JSON.stringify(val))
  } else {
    localStorage.removeItem(storageKey)
  }
  // report new session is saved successfully
  setTimeout(() => {
    app.ports.onStoreChanges.send(val)
  }, 0)
})

window.addEventListener('storage', (event) => {
  if (event.storageArea === localStorage && event.key === storageKey) {
    app.ports.onStoreChanges.send(event.newValue)
  }
}, false)
