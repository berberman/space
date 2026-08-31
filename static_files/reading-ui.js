(() => {
  var header = document.querySelector(".site-header");
  var main = document.querySelector("main");
  var reduceMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)")?.matches || false;
  var ticking = false;
  var lastY = window.scrollY || window.pageYOffset || 0;
  var control = document.createElement("button");

  control.type = "button";
  control.className = "reading-progress";
  control.setAttribute("aria-label", "Back to top");
  control.innerHTML = '<svg class="reading-progress__icon" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M4 12l1.41 1.41L11 7.83V20h2V7.83l5.59 5.58L20 12l-8-8-8 8z"/></svg>';
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
    var scrollingUp = currentY < lastY;
    var pastIntro = currentY > 96;

    if (header) {
      header.style.setProperty("--reading-progress", String(progress / 100));
    }

    control.classList.toggle("is-visible", pageScrollable && pastIntro && scrollingUp);
    lastY = currentY;

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
