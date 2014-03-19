var mash = [ 'cmd', 'alt', 'ctrl' ],
    mashMove = [ 'alt', 'ctrl' ],
    nudgePixels = 10,
    padding = 4,
    previousSizes = {};

// Positioning

api.bind( 'space', mash, function() {
   Window.focusedWindow().toggleFullscreen();
});

api.bind( 'q', mash, function() {
   Window.focusedWindow().toNW();
});
api.bind( 'w', mash, function() {
   Window.focusedWindow().toN();
});
api.bind( 'e', mash, function() {
   Window.focusedWindow().toNE();
});

api.bind( 'a', mash, function() {
   Window.focusedWindow().toW();
});
api.bind( 's', mash, function() {
   Window.focusedWindow().toCenter();
});
api.bind( 'd', mash, function() {
   Window.focusedWindow().toE();
});

api.bind( 'z', mash, function() {
   Window.focusedWindow().toSW();
});
api.bind( 'x', mash, function() {
   Window.focusedWindow().toS();
});
api.bind( 'c', mash, function() {
   Window.focusedWindow().toSE();
});

// Nudging

api.bind( 'up', mash, function() {
   Window.focusedWindow().nudgeUp();
});
api.bind( 'down', mash, function() {
   Window.focusedWindow().nudgeDown();
});
api.bind( 'right', mash, function() {
   Window.focusedWindow().nudgeRight();
});
api.bind( 'left', mash, function() {
   Window.focusedWindow().nudgeLeft();
});

// // Layouts

api.bind( '1', mash, function() {
   var chromeWindow = App.findByTitle('Google Chrome').findWindowNotMatchingTitle('^Developer Tools -'),
   emacsWindow = App.findByTitle('iTerm').firstWindow();
   api.alert( 'Emacs + Chrome', 0.25 );
   if ( emacsWindow ) {
      sublimeWindow.toW();
   }
   if ( chromeWindow ) {
      chromeWindow.toE();
   }
});

// // Helpers

Window.prototype.toGrid = function( x, y, width, height ) {
   var screen = this.screen().frameWithoutDockOrMenu();
   this.setFrame({
      x: Math.round( x * screen.width ) + padding + screen.x,
      y: Math.round( y * screen.height ) + padding + screen.y,
      width: Math.round( width * screen.width ) - ( 2 * padding ),
      height: Math.round( height * screen.height ) - ( 2 * padding )
   });
   this.focusWindow();
   return this;
};

Window.prototype.toFullScreen = function() {
   return this.toGrid( 0, 0, 1, 1 );
};
Window.prototype.toN = function() {
   return this.toGrid( 0, 0, 1, 0.5 );
};
Window.prototype.toNE = function() {
   return this.toGrid( 0.5, 0, 0.5, 0.5 );
};
Window.prototype.toE = function() {
     return this.toGrid( 0.5, 0, 0.5, 1 );
};
Window.prototype.toSE = function() {
     return this.toGrid( 0.5, 0.5, 0.5, 0.5 );
};
Window.prototype.toS = function() {
     return this.toGrid( 0, 0.5, 1, 0.5 );
};
Window.prototype.toSW = function() {
   return this.toGrid( 0, 0.5, 0.5, 0.5 );
};
Window.prototype.toW = function() {
   return this.toGrid( 0, 0, 0.5, 1 );
};
Window.prototype.toNW = function() {
   return this.toGrid( 0, 0, 0.5, 0.5 );
};
Window.prototype.toggleFullscreen = function() {
   if ( previousSizes[ this ] ) {
      this.setFrame( previousSizes[ this ] );
      delete previousSizes[ this ];
   }
   else {
      previousSizes[ this ] = this.frame();
      this.toFullScreen();
   }
   return this;
};


Window.prototype.nudgeLeft = function( factor ) {
   var win = Window.focusedWindow(),
   frame = win.frame(),
   pixels = nudgePixels * ( factor || 1 );
   frame.x -= ( frame.x >= pixels ) ? pixels : 0;
   win.setFrame( frame );
};
Window.prototype.nudgeRight = function( factor ) {
   var win = Window.focusedWindow(),
   frame = win.frame(),
   maxLeft = win.screen().frameIncludingDockAndMenu().width - frame.width,
   pixels = nudgePixels * ( factor || 1 );
   frame.x += ( frame.x < maxLeft - pixels ) ? pixels : 0;
   win.setFrame( frame );
};
Window.prototype.nudgeUp = function( factor ) {
   var win = Window.focusedWindow(),
   frame = win.frame(),
   pixels = nudgePixels * ( factor || 1 );
   frame.y -= ( frame.y >= pixels ) ? pixels : 0;
   win.setFrame( frame );
};
Window.prototype.nudgeDown = function( factor ) {
   var win = Window.focusedWindow(),
   frame = win.frame(),
   maxLeft = win.screen().frameIncludingDockAndMenu().height - frame.height,
   pixels = nudgePixels * ( factor || 1 );
   frame.y += ( frame.y < maxLeft - pixels ) ? pixels : 0;
   win.setFrame( frame );
};

App.findByTitle = function( title ) {
   return _( this.runningApps() ).find( function( app ) {
      if ( app.title() === title ) {
         app.show();
         return true;
      }
   });
};

App.prototype.findWindowMatchingTitle = function( title ) {
   var regexp = new RegExp( title );
   return _( this.visibleWindows() ).find( function( win ) {
      return regexp.test( win.title() );
   });
};
App.prototype.firstWindow = function() {
   return this.visibleWindows()[ 0 ];
};
