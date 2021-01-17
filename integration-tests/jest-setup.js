import '@testing-library/jest-dom'
const WS = require('ws');

global.WebSocket = (url) => new WS(url);
global.console.print = global.console.log;
global.console.log = () => {};