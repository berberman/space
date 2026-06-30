(() => {
  var header = document.querySelector(".site-header");
  var main = document.querySelector("main");
  var reduceMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)")?.matches || false;
  var ticking = false;
  var control = document.createElement("button");
  var percent = document.createElement("span");
  var arrow = document.createElement("span");

  control.type = "button";
  control.className = "reading-progress";
  control.setAttribute("aria-label", "Back to top, 0% read");

  percent.className = "reading-progress__percent";
  percent.textContent = "0%";

  arrow.className = "reading-progress__arrow";
  arrow.setAttribute("aria-hidden", "true");
  arrow.textContent = "↑";

  control.appendChild(percent);
  control.appendChild(arrow);
  document.body.appendChild(control);

  function scrollY() {
    return window.scrollY || window.pageYOffset || 0;
  }

  function documentHeight() {
    return Math.max(
      document.body.scrollHeight,
      document.documentElement.scrollHeight,
      document.body.offsetHeight,
      document.documentElement.offsetHeight
    );
  }

  function readableDistance() {
    return Math.max(0, documentHeight() - window.innerHeight);
  }

  function isMeaningfullyScrollable() {
    return readableDistance() > window.innerHeight * 0.35;
  }

  function update() {
    var currentY = scrollY();
    var distance = readableDistance();
    var pageScrollable = isMeaningfullyScrollable();
    var progress = distance > 0 ? Math.round(Math.min(1, currentY / distance) * 100) : 0;

    if (header) {
      header.classList.toggle("is-scrolled", currentY > 8);
    }

    control.classList.toggle("is-visible", pageScrollable);
    percent.textContent = `${progress}%`;
    control.setAttribute("aria-label", `Back to top, ${progress}% read`);

    ticking = false;
  }

  function requestUpdate() {
    if (!ticking) {
      ticking = true;
      window.requestAnimationFrame(update);
    }
  }

  control.addEventListener("click", () => {
    window.scrollTo({ top: 0, behavior: reduceMotion ? "auto" : "smooth" });
  });

  window.addEventListener("scroll", requestUpdate, { passive: true });
  window.addEventListener("resize", requestUpdate);
  window.addEventListener("load", requestUpdate);

  if (main && "MutationObserver" in window) {
    new MutationObserver(requestUpdate).observe(main, { childList: true, subtree: true });
  }

  update();
})();
