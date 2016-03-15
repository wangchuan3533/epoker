(function() {
  requirejs.config({
    baseUrl: '/public/lib',
    paths: {
      app: '../app'
    }
  });

  requirejs(['app/main']);

}).call(this);
