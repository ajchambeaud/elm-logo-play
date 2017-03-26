require('./main.css');
var Elm = require('./Main.elm');
var config = require('./config');

var root = document.getElementById('root');

firebase.initializeApp(config);

var provider = new firebase.auth.TwitterAuthProvider();

// Database
var logosListRef = firebase.database().ref('/logos');

var logoItemRef = function(userid) {
  return firebase.database().ref('/logos/' + userid);
};

var registerLogoListener = function(user) {
  return logoItemRef(user.uid).on('value', function(logoSnapshot) {
    console.log('Value update');
    console.log(logoSnapshot.val());
    user.customLogo = logoSnapshot.val() || null;
    var userStr = JSON.stringify(user);
    window.localStorage.user = userStr;
    app.ports.loginSuccess.send(user);
  });
};

var logoListRef = firebase.database().ref('/logos/');

logoListRef.on('value', function(logosSnapshot) {
  console.log('list was updated');
  const logos = logosSnapshot.val() || {};
  const list = Object.keys(logos).map(function(userKey) {
    return logos[userKey];
  });
  console.log(list);
  app.ports.customLogosUpdated.send(list);
});

// Check if there is a user in localStorage manually as firebase doesnt have a way to do it in a sync way.
var user = window.localStorage.user && JSON.parse(window.localStorage.user);

if (user) {
  registerLogoListener(user);
}

// Start the elm app without user'
root.innerHTML = ''; // I need to remove the innerHTML, for some reason, embed doesn't do that
var app = Elm.Main.embed(root, user || null);

// This starts the redirection to the Twitter provider and is called on demand via ports
app.ports.login.subscribe(function() {
  firebase.auth().signInWithRedirect(provider);
});

app.ports.logout.subscribe(function() {
  firebase.auth().signOut().then(function() {
    window.localStorage.clear();
    console.log('User logged out correctly');
  }, function(error) {
    console.error('Unespected error in firebase logout');
    window.localStorage.clear();
  });
});

app.ports.savePort.subscribe(function(logo) {
  var user = window.localStorage.user && JSON.parse(window.localStorage.user);
  console.log('About to save logo for user ' + user.uid);
  logoItemRef(user.uid).set(logo);
});

// This callback is called both when returns from provider redirection or when reload the page.
firebase
  .auth()
  .getRedirectResult()
  .then(function(result) {
    var userResult = result.user;

    if (userResult) {
      /*
      * This will be called only when we enter the page redirected from the auth provider site
      * user is empty is there is no redirection proccess pending, so we ignore that case.
      */

      const userData = {};
      userData.uid = userResult.uid;
      userData.displayName = userResult.displayName;
      registerLogoListener(userData);
    }
  })
  .catch(function(error) {
    window.localStorage.clear();
    app.ports.loginError.send(error);
  });
