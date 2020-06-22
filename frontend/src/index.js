import './styles/main.scss';
import { Elm } from './elm/Main.elm';
import { init_flags, storeUserInfo, deleteItem } from "./typescript/WebStorage.ts";
import { initWebSocket } from "./typescript/websocket";
import * as serviceWorker from './serviceWorker';
import 'bootstrap';

let app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: init_flags()
});

app.ports.storeUserInfo.subscribe(storeUserInfo);

app.ports.deleteItem.subscribe(deleteItem);

let webSocketService = null;

app.ports.initWebSocket.subscribe((wsUrl) => {
  console.log("Opening ws connection to: ", wsUrl);
  webSocketService = initWebSocket(wsUrl, {
    onSync(data) { app.ports.onWSSync.send(JSON.stringify(data)) },
    onOffline() { app.ports.onWSOffline.send("") },
    onOnline() { app.ports.onWSOnline.send("") },
    onIncomingMessage(data) { app.ports.incomingWSMessage.send(data) }
  })
});

app.ports.sendWSMessage.subscribe((message) => {
  webSocketService.sendMessage(message).then(
      (reply) => app.ports.replyWSMessage.send(JSON.stringify({message, reply}))
  )
});

app.ports.closeWS.subscribe((message) => {
  webSocketService.close();
  webSocketService = null;
});

app.ports.doLog.subscribe(([prefix, toLog]) => {
  if (toLog !== "") {
    try { toLog = JSON.parse(toLog); } catch (e) {}
    console.log(`${prefix};`, toLog)
  } else {
    console.log(prefix)
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
