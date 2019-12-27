function init_keys() {
  function add_stylesheet(content) {
    var style = document.createElement('style');
    style.innerHTML = content;
    document.head.appendChild(style);
    return style.sheet;
  }

  var context_visibility_style = null;
  function toggle_context_visibility() {
    if (!context_visibility_style) {
      context_visibility_style =
        add_stylesheet(".artefact .context { display: none; }");
      context_visibility_style.disabled = false;
    }
    else {
      context_visibility_style.disabled = !context_visibility_style.disabled;
    }
  }

  var context_selection_style = null;
  function toggle_context_selection() {
    if (!context_selection_style) {
      context_selection_style =
        add_stylesheet(".artefact .context { -webkit-user-select: none; user-select: none }");
      context_selection_style.disabled = false;
    }
    else {
      context_selection_style.disabled = !context_selection_style.disabled;
    }
  }

  window.onkeydown = function (e) {
    if (!e.repeat && !e.ctrlKey && !e.altKey && !e.shiftKey && !e.metaKey) {
      switch (e.key) {
      case "c":
        toggle_context_visibility();
        break;
      case "s":
        toggle_context_selection();
        break;
      }
    }
  };
}

function init_websocket() {
  let websocket = null;
  let keepalive_timer = null;

  function do_send(message) {
    if (websocket && websocket.readyState === WebSocket.OPEN) {
      websocket.send(message);
    }
    else {
      console.log("Not sending a message: socket is not OPEN");
    }
  }

  function ping() {
    do_send(JSON.stringify({type: "ping"}));
  }

  function addHit(message, animate=true) {
    let container = document.getElementById("output");
    if (50 <= container.childElementCount) {
      container.lastChild.remove();
    }

    let stub = document.createElement("div");
    stub.innerHTML = message;
    let hit = stub.firstChild;
    container.insertBefore(hit, container.firstChild);
    if (animate && hit.animate) {
      hit.animate([{ height: "0px",
                     transform: "scaleY(0)" },
                   { height: hit.clientHeight + "px",
                     transform: "scaleY(1)" }],
                  { duration: 500,
                    easing: "ease-out" });
    }
  }

  function uniquify_url(str, param="x") {
    let index = str.indexOf("?");
    let base = 0 < index ? str.slice(0, index) : str;
    let d = new Date().toJSON().replace(/[^\d]/g, "");
    return base + "?" + param + "=" + d;
  }

  function connect() {
    let url = "ws://" + window.location.host + "/ws";
    let conn = new WebSocket(url);

    conn.onerror = function(evt) {
      console.log("Websocket error: ", evt);
    };

    conn.onopen = function(evt) {
      if (websocket) {
        console.log("Unexpectedly connected additional socket", evt.target);
      }
      else {
        console.log("Connected", evt.target);
        websocket = evt.target;
        if (keepalive_timer) {
          console.log("keepalive_timer active in conn.onopen!");
          window.clearInterval(keepalive_timer);
        }
        keepalive_timer = window.setInterval(ping, 150 * 1000);
      }
    };

    conn.onclose = function(evt) {
      if (!websocket || evt.target === websocket) {
        console.log("Disconnected", evt.target);
        websocket = null;
        window.clearInterval(keepalive_timer);
        keepalive_timer = null;

        window.setTimeout(connect, 10 * 1000);
      }
      else {
        console.log("Disconnected dangling socket", evt.target);
      }
    };

    conn.onmessage = function(evt) {
      let msg = JSON.parse(evt.data);
      if ("type" in msg) {
        switch (msg.type) {
        case "add_hit":
          addHit(msg.data);
          break;
        case "bulk_add_hit":
          addHit(msg.data, false);
          break;
        case "reload":
          // Reload the Page itself.
          window.location.reload();
          break;
        case "reload-css":
          // Reload CSS.
          let links = document.getElementsByTagName("link");
          for (let i = 0; i < links.length; i++) {
            let link = links[i];
            if (link.getAttribute("type") == "text/css") {
              link.href = uniquify_url(link.href);
            }
          }
          break;
        case "reload-js":
          // Reload scripts.
          let scripts = document.getElementsByTagName("script");
          for (let i = 0; i < scripts.length; i++) {
            let script = scripts[i];
            script.src = uniquify_url(script.src);
          }
          break;
        case "pong":
          // Do nothing for now.
          break;
        default:
          console.log("Don't know how to handle", msg);
          break;
        }
      }
      else {
        console.log("Received malformed message", msg);
      }
    };

    return conn;
  }

  connect();
}

function init() {
  init_websocket();
  init_keys();
}

window.addEventListener("load", init, false);
