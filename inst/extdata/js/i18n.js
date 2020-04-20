function translate_all() {
    var items = document.querySelectorAll("[lang_key]")
    for (var i=0; i < items.length; i++) { try { items[i].innerHTML = mytr(items[i].getAttribute("lang_key")); } catch(error) {items[i].innerHTML = items[i].getAttribute("lang_key"); }}
}

/* i18njs Internationalisation library for JS projects MIT License Copyright (c) 2013-2018 Simon Rodwell  */
(function(){var e,t,n,r=function(e,t){return function(){return e.apply(t,arguments)}};e=function(){function e(){this.translate=r(this.translate,this);this.data={values:{},contexts:[]};this.globalContext={}}e.prototype.translate=function(e,t,n,r,i){var s,o,u,a;if(i==null){i=this.globalContext}u=function(e){var t;t=typeof e;return t==="function"||t==="object"&&!!e};if(u(t)){s=null;a=null;o=t;i=n||this.globalContext}else{if(typeof t==="number"){s=null;a=t;o=n;i=r||this.globalContext}else{s=t;if(typeof n==="number"){a=n;o=r;i=i}else{a=null;o=n;i=r||this.globalContext}}}if(u(e)){if(u(e["i18n"])){e=e["i18n"]}return this.translateHash(e,i)}else{return this.translateText(e,a,o,i,s)}};e.prototype.add=function(e){var t,n,r,i,s,o,u,a;if(e.values!=null){o=e.values;for(n in o){r=o[n];this.data.values[n]=r}}if(e.contexts!=null){u=e.contexts;a=[];for(i=0,s=u.length;i<s;i++){t=u[i];a.push(this.data.contexts.push(t))}return a}};e.prototype.setContext=function(e,t){return this.globalContext[e]=t};e.prototype.clearContext=function(e){return this.lobalContext[e]=null};e.prototype.reset=function(){this.data={values:{},contexts:[]};return this.globalContext={}};e.prototype.resetData=function(){return this.data={values:{},contexts:[]}};e.prototype.resetContext=function(){return this.globalContext={}};e.prototype.translateHash=function(e,t){var n,r;for(n in e){r=e[n];if(typeof r==="string"){e[n]=this.translateText(r,null,null,t)}}return e};e.prototype.translateText=function(e,t,n,r,i){var s,o;if(r==null){r=this.globalContext}if(this.data==null){return this.useOriginalText(i||e,t,n)}s=this.getContextData(this.data,r);if(s!=null){o=this.findTranslation(e,t,n,s.values,i)}if(o==null){o=this.findTranslation(e,t,n,this.data.values,i)}if(o==null){return this.useOriginalText(i||e,t,n)}return o};e.prototype.findTranslation=function(e,t,n,r){var i,s,o,u,a;o=r[e];if(o==null){return null}if(t==null){if(typeof o==="string"){return this.applyFormatting(o,t,n)}}else{if(o instanceof Array||o.length){for(u=0,a=o.length;u<a;u++){s=o[u];if((t>=s[0]||s[0]===null)&&(t<=s[1]||s[1]===null)){i=this.applyFormatting(s[2].replace("-%n",String(-t)),t,n);return this.applyFormatting(i.replace("%n",String(t)),t,n)}}}}return null};e.prototype.getContextData=function(e,t){var n,r,i,s,o,u,a,f;if(e.contexts==null){return null}a=e.contexts;for(o=0,u=a.length;o<u;o++){n=a[o];r=true;f=n.matches;for(i in f){s=f[i];r=r&&s===t[i]}if(r){return n}}return null};e.prototype.useOriginalText=function(e,t,n){if(t==null){return this.applyFormatting(e,t,n)}return this.applyFormatting(e.replace("%n",String(t)),t,n)};e.prototype.applyFormatting=function(e,t,n){var r,i;for(r in n){i=new RegExp("%{"+r+"}","g");e=e.replace(i,n[r])}return e};return e}();n=new e;t=n.translate;t.translator=n;t.create=function(n){var r;r=new e;if(n!=null){r.add(n)}r.translate.create=t.create;return r.translate};(typeof module!=="undefined"&&module!==null?module.exports=t:void 0)||(this.i18n=t)}).call(this)