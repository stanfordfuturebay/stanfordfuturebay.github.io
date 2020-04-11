<html>
  <body>
    <script>
      (async () => {
        const response = await fetch('https://api.github.com/repos/stanfordfuturebay/stanfordfuturebay.github.io/contents/');
        const data = await response.json();
        let htmlString = 'test<ul>';
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
