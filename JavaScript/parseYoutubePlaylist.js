var allLinks = $("ol").getElementsByTagName("li");

for (var i=0, il=allLinks.length; i<il; i++) {
  console.log(allLinks[i].getElementsByTagName("a")[0].title);
  console.log(allLinks[i].getElementsByTagName("a")[0].href);
}
