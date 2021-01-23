import '@testing-library/jest-dom'
const WS = require('ws');

global.WebSocket = (url) => new WS(url);
global.console.log = () => {};