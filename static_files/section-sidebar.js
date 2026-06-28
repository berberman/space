(function () {
  function normalizePath(pathname) {
    if (!pathname) return "/";
    var normalized = pathname.replace(/\/+$/, "");
    return normalized || "/";
  }

  function markCurrentTopNav() {
    var currentPath = normalizePath(window.location.pathname);
    var topLinks = Array.prototype.slice.call(document.querySelectorAll("nav.top a[href]"));

    topLinks.forEach(function (link) {
      var linkPath;

      try {
        linkPath = normalizePath(new URL(link.href || link.getAttribute("href"), document.baseURI || window.location.href).pathname);
      } catch {
        linkPath = "";
      }

      var descendantPrefix = linkPath.concat("/");
      var active = linkPath === "/"
        ? currentPath === "/"
        : currentPath === linkPath || currentPath.indexOf(descendantPrefix) === 0;

      link.classList.toggle("is-current", active);
      if (active) {
        link.setAttribute("aria-current", "page");
      } else {
        link.removeAttribute("aria-current");
      }
    });
  }

  markCurrentTopNav();

  var sidebar = document.querySelector(".section-sidebar");
  if (!sidebar) return;

  var links = Array.prototype.slice.call(sidebar.querySelectorAll("a[data-section-id]"));
  if (links.length === 0) return;

  var byId = {};
  var headings = links
    .map(function (link) {
      var id = link.getAttribute("data-section-id");
      if (!id) return null;
      var heading = document.getElementById(id);
      if (!heading) return null;
      byId[id] = link;
      return heading;
    })
    .filter(Boolean);

  if (headings.length === 0) return;

  function setCurrent(id) {
    links.forEach(function (link) {
      var active = link.getAttribute("data-section-id") === id;
      link.classList.toggle("is-current", active);
      if (active) {
        link.setAttribute("aria-current", "true");
      } else {
        link.removeAttribute("aria-current");
      }
    });
  }

  function scrollOffset() {
    var styles = window.getComputedStyle(document.documentElement);
    var padding = parseFloat(styles.scrollPaddingTop || styles.getPropertyValue("scroll-padding-top"));
    return Number.isFinite(padding) ? padding : 0;
  }

  function currentByScroll() {
    var current = headings[0];
    var activationLine = scrollOffset() + 80;

    headings.forEach(function (heading) {
      if (heading.getBoundingClientRect().top <= activationLine) {
        current = heading;
      }
    });

    setCurrent(current.id);
  }

  links.forEach(function (link) {
    link.addEventListener("click", function () {
      var id = link.getAttribute("data-section-id");
      if (id) {
        setCurrent(id);
      }
    });
  });

  var visible = {};
  var observer;

  if ("IntersectionObserver" in window) {
    observer = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            visible[entry.target.id] = entry.boundingClientRect.top;
          } else {
            delete visible[entry.target.id];
          }
        });

        var currentId = Object.keys(visible).sort(function (left, right) {
          return visible[left] - visible[right];
        })[0];

        if (currentId && byId[currentId]) {
          setCurrent(currentId);
        } else {
          currentByScroll();
        }
      },
      {
        rootMargin: "-20% 0px -65% 0px",
        threshold: 0
      }
    );

    headings.forEach(function (heading) {
      observer.observe(heading);
    });
  }

  currentByScroll();
  window.addEventListener("scroll", currentByScroll, { passive: true });
  window.addEventListener("resize", currentByScroll);
})();
