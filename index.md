<html>
  <body>
    <script>
      (async () => {
        const response = await fetch('https://api.github.com/repos/stanfordfuturebay/stanfordfuturebay.github.io/contents/');
        const data = await response.json();
        let htmlString = "<h1>Stanford Future Bay Initiative: COVID-19 Rapid Response Projects</h1>The following is a collection of data exercises conducted by teaching staff and students in the Spring 218Z course organized by the Future Bay Initiative at Stanford. The formal public face of our work is at bay.stanford.edu/covid19. This is more for our internal collaboration and sharing with technical partners. The list below just automatically updates with the HTML files that we have pushed to our public GitHub repo. We have a separate private repo for code/data we can't publish. If you would like to learn more about what we are doing or get involved, reach out to Derek (contact info at bay.stanford.edu).<br><br><ul>";
        for (let file of data) {
          if(file.name != "_config.yml" & file.name != "index.md" & file.name != ".DS_Store" & file.name != "404.html") {
          htmlString += `<li><a href="${file.path}">${file.name}</a></li>`;
      }
        }
        htmlString += '</ul>';
        document.getElementsByTagName('body')[0].innerHTML = htmlString;
      })()
    </script>
    
  <body>
</html>
