// tooltips.js ---Tooltips for org-special-block-extras: hover on a desktop, tap on a phone.
//
// By way of example, the exported Org `[[doc:cat]]` becomes
//
//     <abbr class="tooltip" tabindex="0">Category Theory<template><p>A theory of composition.</p></template></abbr>
//
// and we show the <template>'s HTML in one shared box, positioned beside the word.  The box is ordinary HTML, so its
// links are clickable: hovering from the word into the box keeps it open.
//
// We replace tooltipster ---and the jQuery it needs--- since it opened only on mouseenter, which no phone fires, and
// fetched its bundle cross-origin per page; see https://github.com/alhassy/org-special-block-extras/issues/53.
// This file is inlined into each exported page, sans any request.
//
// | Action                                   | Effect                    |
// |------------------------------------------+---------------------------|
// | Hover a word, with a mouse               | Open, after a beat        |
// | Leave the word and the box               | Close, after a beat       |
// | Tap, or click, a word                    | Open; again to close      |
// | Tap, or click, anywhere else; or Escape  | Close                     |
// | Tab onto a word                          | Open; Escape to close     |
document.addEventListener('DOMContentLoaded', () => {
  const box = document.createElement('div');
  box.className = 'tooltip-box';
  box.setAttribute('role', 'tooltip');
  box.hidden = true;
  document.body.append(box);

  let owner = null; // The .tooltip whose box is open, if any.
  let timer;        // A pending open or close; the newest intent wins.
  const later = (action, ms) => { clearTimeout(timer); timer = setTimeout(action, ms); };

  const open = (word) => {
    clearTimeout(timer);
    if (owner === word) return;
    close();
    owner = word;
    box.innerHTML = tipOf(word)?.innerHTML ?? '';
    box.hidden = false;
    place(word);
  };

  const close = () => { clearTimeout(timer); owner = null; box.hidden = true; };

  // A word's tip is its <template>; else, in maths or a graph's <svg>, which keep no <template> within, the one its
  // osbe-tip-ID class names, beside them: \class{tooltip osbe-tip-g1}{x} … <template id="osbe-tip-g1">.
  const tipOf = (word) => word.querySelector(':scope > template')
    ?? document.getElementById([...word.classList].find((c) => c.startsWith('osbe-tip-')));

  // Above the word when there is room, else below; centred on it, yet never past the screen's edges ---a phone's
  // screen being narrower than most tooltips.
  const place = (word) => {
    const at = word.getBoundingClientRect(), gap = 8;
    const centred = at.left + at.width / 2 - box.offsetWidth / 2;
    const left = Math.min(Math.max(gap, centred), innerWidth - box.offsetWidth - gap);
    const above = at.top > box.offsetHeight + gap;
    box.style.left = `${left + scrollX}px`;
    box.style.top = `${(above ? at.top - box.offsetHeight - gap : at.bottom + gap) + scrollY}px`;
  };

  const wordAt = (node) => node instanceof Element ? node.closest('.tooltip') : null;

  // Hover is for mice alone: a phone fires pointerover on each tap, which the click handler below already serves.
  document.addEventListener('pointerover', (e) => {
    if (e.pointerType !== 'mouse') return;
    const word = wordAt(e.target);
    if (word) later(() => open(word), 100);
    else if (box.contains(e.target)) clearTimeout(timer);
  });
  document.addEventListener('pointerout', (e) => {
    if (e.pointerType !== 'mouse' || !owner) return;
    const into = e.relatedTarget;
    if (!(box.contains(into) || wordAt(into) === owner)) later(close, 300);
  });

  document.addEventListener('click', (e) => {
    if (box.contains(e.target)) return; // A link in the box does as links do.
    const word = wordAt(e.target);
    if (word && word !== owner) open(word);
    else close();
  });

  // A tap focuses the word too; only keyboard focus, which is visible, opens it ---else the tap's click would close it.
  document.addEventListener('focusin', (e) => {
    const word = wordAt(e.target);
    if (word?.matches(':focus-visible')) open(word);
  });
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') close(); });
  // A phone resizes the page whenever its address bar slides away mid-scroll; so we follow the word, not close.
  addEventListener('resize', () => owner && place(owner));
});
