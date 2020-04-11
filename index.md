<html>
  <body>
    <script>
      (async () => {
        const response = await fetch('https://api.github.com/repos/stanfordfuturebay/stanfordfuturebay.github.io/contents/');
        const data = await response.json();
        let htmlString = "The following is a collection of data exercises conducted by teaching staff and students in the Spring 218Z course organized by the <a href = "http://bay.stanford.edu/">Future Bay Initiative</a> at Stanford. We do not have an official front page to our project work yet. The list below just automatically updates with the HTML files that we have pushed to our GitHub repo and is primarily meant for internal team navigation. If you would like to learn more about what we are doing or get involved, reach out to Derek (contact info at bay.stanford.edu).<br><ul>";
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
