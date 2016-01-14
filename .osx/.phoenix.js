'use strict';

var keys = [];
var mash = [ 'ctrl', 'alt', 'cmd' ];

function alert(message) {
  var modal = new Modal();
  modal.message = message;
  modal.duration = 2;
  modal.show();
}

function callApp(appName) {
  var app = App.launch(appName);
  app.focus();
  var window = _.first(app.windows());
  Mouse.moveTo({
    x: window.topLeft().x + window.frame().width / 2,
    y: window.topLeft().y + window.frame().height / 2
  });
}

Window.prototype.toGrid = function(x, y, width, height) {
  var frame = this.screen().visibleFrameInRectangle();
  var extrax = 0;
  if (((height == 1.0) || (height == 0.5)) && (x == 0.0) && (frame.x == 4)) {
    extrax = 4;
  }
  this.setFrame({
    x: Math.round( x * frame.width ) + frame.x - extrax,
    y: Math.round( y * frame.height ) + frame.y,
    width: Math.round( width * frame.width ) + extrax,
    height: Math.round( height * frame.height )
  });
}

keys.push(Phoenix.bind('q', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0, 0.5, 0.5);
}));
keys.push(Phoenix.bind('w', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0, 1, 0.5);
}));
keys.push(Phoenix.bind('e', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0.5, 0, 0.5, 0.5);
}));
keys.push(Phoenix.bind('a', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0, 0.5, 1);
}));
keys.push(Phoenix.bind('s', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0.1, 0.1, 0.8, 0.8);
}));
keys.push(Phoenix.bind('d', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0.5, 0, 0.5, 1);
}));
keys.push(Phoenix.bind('z', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0.5, 0.5, 0.5);
}));
keys.push(Phoenix.bind('x', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0.5, 1, 0.5);
}));
keys.push(Phoenix.bind('c', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0.5, 0.5, 0.5, 0.5);
}));
keys.push(Phoenix.bind('space', mash, function () {
  Window.focusedWindow() && Window.focusedWindow().toGrid(0, 0, 1, 1);
}));

keys.push(Phoenix.bind('i', mash, function() { callApp('iTerm'); }));
keys.push(Phoenix.bind('g', mash, function() { callApp('Emacs'); }));
keys.push(Phoenix.bind('n', mash, function() { callApp('Google Chrome'); }));
keys.push(Phoenix.bind('b', mash, function() { callApp('Safari'); }));
keys.push(Phoenix.bind('t', mash, function() { callApp('TweetDeck'); }));
keys.push(Phoenix.bind('l', mash, function() { callApp('Mail'); }));
keys.push(Phoenix.bind('r', mash, function() { callApp('Calendar'); }));
keys.push(Phoenix.bind('k', mash, function() { callApp('Slack'); }));
keys.push(Phoenix.bind('m', mash, function() { callApp('Messages'); }));
keys.push(Phoenix.bind('y', mash, function() { callApp('Skype'); }));
keys.push(Phoenix.bind('o', mash, function() { callApp('iTunes'); }));
keys.push(Phoenix.bind('p', mash, function() { callApp('KeePassX'); }));
keys.push(Phoenix.bind('.', mash, function() { callApp('Activity Monitor'); }));
keys.push(Phoenix.bind('/', mash, function() { callApp('Finder'); }));
