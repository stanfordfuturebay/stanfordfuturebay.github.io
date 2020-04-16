<html>
  
          <script type="text/javascript">
      // Single Page Apps for GitHub Pages
      // https://github.com/rafrex/spa-github-pages
      // Copyright (c) 2016 Rafael Pedicini, licensed under the MIT License
      // ----------------------------------------------------------------------
      // This script checks to see if a redirect is present in the query string
      // and converts it back into the correct url and adds it to the
      // browser's history using window.history.replaceState(...),
      // which won't cause the browser to attempt to load the new url.
      // When the single page app is loaded further down in this file,
      // the correct url will be waiting in the browser's history for
      // the single page app to route accordingly.
      (function(l) {
        if (l.search) {
          var q = {};
          l.search.slice(1).split('&').forEach(function(v) {
            var a = v.split('=');
            q[a[0]] = a.slice(1).join('=').replace(/~and~/g, '&');
          });
          if (q.p !== undefined) {
            window.history.replaceState(null, null,
              l.pathname.slice(0, -1) + (q.p || '') +
              (q.q ? ('?' + q.q) : '') +
              l.hash
            );
          }
        }
      }(window.location))
    </script>
    
  
  <body>
    

    <script>
      (async () => {
        const response = await fetch('https://api.github.com/repos/stanfordfuturebay/stanfordfuturebay.github.io/contents/data');
        const data = await response.json();
        let htmlString = "<h1>Stanford Future Bay Initiative: COVID-19 Rapid Response Projects</h1>The following is a collection of data exercises conducted by teaching staff and students in the Spring 218Z course organized by the Future Bay Initiative at Stanford. We do not have an official front page to our project work yet. The list below just automatically updates with the HTML files that we have pushed to our GitHub repo and is primarily meant for internal team navigation. If you would like to learn more about what we are doing or get involved, reach out to Derek (contact info at bay.stanford.edu).<br><br><h2>Data</h2><i>Click Back on your browser to return to the home directory.</i><br><br><ul>";
        for (let file of data) {
          if(file.name != "_config.yml" & file.name != "index.md" & file.name != ".DS_Store" % file.name != "404.html") {
          htmlString += `<li><a href="${file.path}">${file.name}</a></li>`;
      }
        }
        htmlString += '</ul>';
        document.getElementsByTagName('body')[0].innerHTML = htmlString;
      })()
    </script>
    
  <body>
</html>
