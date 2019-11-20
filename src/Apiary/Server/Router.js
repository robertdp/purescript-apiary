var Router = require("find-my-way");

exports._create = function(defaultRoute) {
  return Router({ defaultRoute: defaultRoute });
};

exports._on = function(router, method, path, handler) {
  router.on(method, path, handler);
};

exports._lookup = function(router, req, res) {
  router.lookup(req, res);
};
