import './main.css';
import { Elm } from './Main.elm';
//import '../node_modules/elm-debug-transformer/dist\
//elm-console-debug.js';
//import registerServiceWorker from './registerServiceWorker';

import * as ElmDebugger from 'elm-debug-transformer';

ElmDebugger.register();

Elm.Main.init({
  node: document.getElementById('root')
});

//registerServiceWorker();
