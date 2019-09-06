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

customElements.define
('editable-span',
 class extends HTMLElement {
     constructor () {
	 super()
	 this._string = 'hello schworld'
     }
     get string () {
	 if (!this._editor) return ""
	 this._string = this._editor.innerHTML
	 return this._string
     }
     set string(value){
	 if (value != this._string ) { this._string = value }
     }
	 
     connectedCallback(){
	 let shadowRoot = this.attachShadow({mode: "open"})
	 shadowRoot.innerHTML = `
	    <style>
	     :host {
		 border: 2px solid #2f4858; border-radius: 3px;
		 background-color: #f0fff0;
		 display: block;
		 padding: 3px; margin: 10px
	     }
	     .toolbar { height: 20px; border-bottom: 1px solid #2f4858 }
	     .string { color: #33658a; padding-top: 3px; }
	    </style>
	    <span class="toolbar">
	     <strong>B</strong>&nbsp;
	     <em>I</em>&nbsp;
	     <span style="text-decoration: underline">U</span>
	    </span>
	    <span class="string" contenteditable='true'>${this.string}</span>
	 `
	 this._editor = this.shadowRoot.querySelector(".string")
	 this._editor.addEventListener('input', (e)=>{
	     //console.log (e)
	     //console.log (e.target.innerHTML)
	     this._string = this._editor.innerHTML
	     this.dispatchEvent (new CustomEvent('stringChanged', { detail: e.target.innerHTML } ))
	 })
	 //this.addEventListener('stringChanged', (e)=>{console.log (e); console.log (e.detail)})
     }
 }			  
)
/**
      constructor() {
	super()

	let shadowRoot = this.attachShadow({mode: "open"})
	let content = this.hasAttribute("string") ? this.getAttribute("string") : ""
	
    } )

    static get observedAttributes() {
	return ["string"]
    }

    attributeChangedCallback(attr, oldVal, newVal) {
	if (attr == "string") {
	    this.value = newVal
	}
    }

    get value() {
	return this.shadowRoot.querySelector(".content").innerHTML
    } 

    set value(val) {
	this.shadowRoot.querySelector(".content").innerHTML = val
	this.dispatchEvent(new CustomEvent("change", {detail: this.innerHTML}))
    } 
}
**/
