"use strict";

var keys = [];
var mash = ["alt", "cmd"];

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
      y: window.topLeft().y + window.frame().height / 2,
    });
  }
}

Window.prototype.toGrid = function (x, y, width, height) {
  var frame = this.screen().visibleFrameInRectangle();
  var gap_x = 3;
  var gap_y = 3;
  this.setFrame({
    x: Math.round(x * frame.width) + frame.x + gap_x,
    y: Math.round(y * frame.height) + frame.y + gap_y,
    width: Math.round(width * frame.width) - gap_x * 2,
    height: Math.round(height * frame.height) - gap_y * 2,
  });
};

keys.push(
  new Key("q", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0, 0.5, 0.5);
  }),
);
keys.push(
  new Key("w", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0, 1, 0.5);
  }),
);
keys.push(
  new Key("e", mash, function () {
    Window.focused() && Window.focused().toGrid(0.5, 0, 0.5, 0.5);
  }),
);
keys.push(
  new Key("a", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0, 0.5, 1);
  }),
);
keys.push(
  new Key("s", mash, function () {
    Window.focused() && Window.focused().toGrid(0.1, 0.1, 0.8, 0.8);
  }),
);
keys.push(
  new Key("=", mash, function () {
    Window.focused() && Window.focused().toGrid(0.05, 0.05, 0.9, 0.9);
  }),
);
keys.push(
  new Key("d", mash, function () {
    Window.focused() && Window.focused().toGrid(0.5, 0, 0.5, 1);
  }),
);
keys.push(
  new Key("z", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0.5, 0.5, 0.5);
  }),
);
keys.push(
  new Key("x", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0.5, 1, 0.5);
  }),
);
keys.push(
  new Key("c", mash, function () {
    Window.focused() && Window.focused().toGrid(0.5, 0.5, 0.5, 0.5);
  }),
);
keys.push(
  new Key("f", mash, function () {
    Window.focused() && Window.focused().toGrid(0, 0, 1, 1);
  }),
);
