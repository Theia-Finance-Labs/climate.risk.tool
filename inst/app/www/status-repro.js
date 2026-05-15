(function() {
  function getCodeText(targetId) {
    var container = document.getElementById(targetId);
    if (!container) {
      return "";
    }

    var pre = container.querySelector("pre");
    if (pre && pre.textContent) {
      return pre.textContent;
    }

    return container.textContent || "";
  }

  function fallbackCopyText(text) {
    var textarea = document.createElement("textarea");
    textarea.value = text;
    textarea.setAttribute("readonly", "");
    textarea.style.position = "fixed";
    textarea.style.top = "-9999px";
    textarea.style.left = "-9999px";
    document.body.appendChild(textarea);
    textarea.focus();
    textarea.select();

    var success = false;
    try {
      success = document.execCommand("copy");
    } catch (err) {
      success = false;
    }

    document.body.removeChild(textarea);
    return success;
  }

  window.copyStatusReproCode = function(button) {
    if (!button) {
      return false;
    }

    var targetId = button.getAttribute("data-copy-target");
    var text = getCodeText(targetId).trimEnd();
    if (!text) {
      return false;
    }

    var originalLabel = button.innerHTML;
    var setButtonState = function(label, modifierClass) {
      button.innerHTML = label;
      button.classList.remove("status-repro-copy-btn--success", "status-repro-copy-btn--error");
      if (modifierClass) {
        button.classList.add(modifierClass);
      }
    };

    var resetButton = function() {
      window.setTimeout(function() {
        button.innerHTML = originalLabel;
        button.classList.remove("status-repro-copy-btn--success", "status-repro-copy-btn--error");
      }, 1500);
    };

    var onSuccess = function() {
      setButtonState('<i class="fa fa-check"></i> Copied', "status-repro-copy-btn--success");
      resetButton();
      return true;
    };

    var onFailure = function() {
      setButtonState('<i class="fa fa-xmark"></i> Copy failed', "status-repro-copy-btn--error");
      resetButton();
      return false;
    };

    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(onSuccess).catch(function() {
        if (fallbackCopyText(text)) {
          onSuccess();
        } else {
          onFailure();
        }
      });
      return true;
    }

    if (fallbackCopyText(text)) {
      return onSuccess();
    }

    return onFailure();
  };
})();
