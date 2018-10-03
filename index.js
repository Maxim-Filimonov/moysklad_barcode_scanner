// import main from "./src/Main.elm";
import("./src/Main.elm").then(({ Elm }) => {
  var app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: localStorage.getItem("token"),
  });
  app.ports.setToken.subscribe(function(token) {
    localStorage.setItem("token", token);
  });
});
