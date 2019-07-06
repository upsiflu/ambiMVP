import './main.css';
import { Elm } from './Main.elm';
//import registerServiceWorker from './registerServiceWorker';

import * as ElmDebugger from 'elm-debug-transformer';

ElmDebugger.register();

Elm.Main.init({
  node: document.getElementById('root')
});

//registerServiceWorker();
