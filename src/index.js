require('./main.css');
var Elm = require('./Main.elm');
var config = require('./config');

var root = document.getElementById('root');

firebase.initializeApp(config);

var provider = new firebase.auth.TwitterAuthProvider();

// Check if there is a user in localStorage manually as firebase doesnt have a way to do it in a sync way.
var user = window.localStorage.user && JSON.parse(window.localStorage.user);

// Start the elm app without user
var app = Elm.Main.embed(root, user || null);


// This starts the redirection to the Twitter provider and is called on demand via ports
app.ports.login.subscribe(() => firebase.auth().signInWithRedirect(provider));

app.ports.logout.subscribe(() => {
  firebase.auth().signOut().then(function() {
    window.localStorage.clear();
    console.log("User logged out correctly");
  }, function(error) {
    console.error("Unespected error in firebase logout");
    window.localStorage.clear();
  });
});

// This callback is called both when returns from provider redirection or when reload the page.
firebase.auth().getRedirectResult().then(function(result) {
  var user = result.user;

  if (user) {
    /*
    * This will be called only when we enter the page redirected from the auth provider site
    * user is empty is there is no redirection proccess pending, so we ignore that case.
    */

    var userStr = JSON.stringify(user);
    window.localStorage.user = userStr;

    app.ports.loginSuccess.send(user);
  }
}).catch(function(error) { 
  window.localStorage.clear();
  app.ports.loginError.send(error);
});
