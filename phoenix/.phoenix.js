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
  var extrax = 0;
  if ((height == 1.0 || height == 0.5) && x == 0.0 && frame.x == 4) {
    extrax = 4;
  }
  this.setFrame({
    x: Math.round(x * frame.width) + frame.x - extrax,
    y: Math.round(y * frame.height) + frame.y,
    width: Math.round(width * frame.width) + extrax,
    height: Math.round(height * frame.height),
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
