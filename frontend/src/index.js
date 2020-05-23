import './styles/main.scss';
import { Elm } from './elm/Main.elm';
import { init_flags, storeUserInfo } from "./typescript/WebStorage.ts";
import * as serviceWorker from './serviceWorker';
import 'bootstrap';

let app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: init_flags()
});

app.ports.storeUserInfo.subscribe(storeUserInfo);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
