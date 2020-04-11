<html>
  <body>
    <script>
      (async () => {
        const response = await fetch('https://api.github.com/repos/stanfordfuturebay/stanfordfuturebay.github.io/contents/');
        const data = await response.json();
        let htmlString = '<h1>Stanford Future Bay Initiative: COVID-19 Rapid Response Projects</h1>The following is a collection of data exercises conducted by teaching staff and students in the Spring CEE/GEOPHYS 218Z course organized by the <a href = "http://bay.stanford.edu/">Future Bay Initiative</a> at Stanford. We do not have an official "front page" to our project work yet; the list below just automatically updates with the HTML files that we've pushed to our <a href = "https://github.com/stanfordfuturebay/stanfordfuturebay.github.io">GitHub repo</a> and is primarily meant for internal file navigation. If you'd like to learn more about what we're doing or get involved, reach out to Derek (contact info at the first link).<ul>';
        for (let file of data) {
          if(file.name != "_config.yml" & file.name != "index.md") {
          htmlString += `<li><a href="${file.path}">${file.name}</a></li>`;
      }
        }
        htmlString += '</ul>';
        document.getElementsByTagName('body')[0].innerHTML = htmlString;
      })()
    </script>
  <body>
</html>
