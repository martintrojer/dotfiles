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
  if (window) {
    Mouse.moveTo({
      x: window.topLeft().x + window.frame().width / 2,
      y: window.topLeft().y + window.frame().height / 2
    });
  }
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

keys.push(new Key('q', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0, 0.5, 0.5);
}));
keys.push(new Key('w', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0, 1, 0.5);
}));
keys.push(new Key('e', mash, function () {
  Window.focused() && Window.focused().toGrid(0.5, 0, 0.5, 0.5);
}));
keys.push(new Key('a', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0, 0.5, 1);
}));
keys.push(new Key('s', mash, function () {
  Window.focused() && Window.focused().toGrid(0.1, 0.1, 0.8, 0.8);
}));
keys.push(new Key('=', mash, function () {
  Window.focused() && Window.focused().toGrid(0.05, 0.05, 0.9, 0.9);
}));
keys.push(new Key('d', mash, function () {
  Window.focused() && Window.focused().toGrid(0.5, 0, 0.5, 1);
}));
keys.push(new Key('z', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0.5, 0.5, 0.5);
}));
keys.push(new Key('x', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0.5, 1, 0.5);
}));
keys.push(new Key('c', mash, function () {
  Window.focused() && Window.focused().toGrid(0.5, 0.5, 0.5, 0.5);
}));
keys.push(new Key('space', mash, function () {
  Window.focused() && Window.focused().toGrid(0, 0, 1, 1);
}));

keys.push(new Key('i', mash, function() { callApp('iTerm'); }));
keys.push(new Key('g', mash, function() { callApp('Emacs'); }));
keys.push(new Key('n', mash, function() { callApp('Google Chrome'); }));
keys.push(new Key('b', mash, function() { callApp('Safari'); }));
keys.push(new Key('t', mash, function() { callApp('TweetDeck'); }));
keys.push(new Key('l', mash, function() { callApp('Mail'); }));
keys.push(new Key('r', mash, function() { callApp('Calendar'); }));
keys.push(new Key('k', mash, function() { callApp('Slack'); }));
keys.push(new Key('m', mash, function() { callApp('Messages'); }));
keys.push(new Key('y', mash, function() { callApp('Skype'); }));
keys.push(new Key('o', mash, function() { callApp('iTunes'); }));
keys.push(new Key('p', mash, function() { callApp('KeePassX'); }));
keys.push(new Key('.', mash, function() { callApp('Activity Monitor'); }));
keys.push(new Key('/', mash, function() { callApp('Finder'); }));
