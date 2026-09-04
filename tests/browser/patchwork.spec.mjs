import { chromium } from 'playwright';

const exe = process.env.CHROME || '/home/dmjio/.nix-profile/bin/google-chrome-stable';
const browser = await chromium.launch({ executablePath: exe, headless: true });
const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
const msgs = [];
page.on('console', (m) => msgs.push(`[${m.type()}] ${m.text()}`));
page.on('pageerror', (e) => msgs.push(`[pageerror] ${e.message}`));

let failures = 0;
const assert = (name, cond, detail = '') => {
  if (cond) console.log(`ok   ${name}`);
  else { failures++; console.log(`FAIL ${name} ${detail}`); }
};
const q = (sel) => page.evaluate((s) => document.querySelectorAll(s).length, sel);
const center = (b) => ({ x: b.x + b.width / 2, y: b.y + b.height / 2 });

await page.goto('http://localhost:8931/index.html');
await page.waitForTimeout(1500);
await page.click('.miso-flow__controls button:nth-child(4)'); // fit
await page.waitForTimeout(700);

// cable tag + both marker kinds in defs
assert('permanent cable tag', (await page.locator('.pw-cable-tag').textContent()) === 'control');
assert('open and closed markers defined', await q('.miso-flow__edges marker') === 2,
  `got ${await q('.miso-flow__edges marker')}`);

// --- 1. validator: osc-a -> out is refused (red line, no edge)
const edges0 = await q('.miso-flow__edge');
const oscSrc = center(await page.locator('.miso-flow__handle.source[data-nodeid="osc-a"]').boundingBox());
const outTgt = center(await page.locator('.miso-flow__handle.target[data-nodeid="out"]').boundingBox());
await page.mouse.move(oscSrc.x, oscSrc.y);
await page.mouse.down();
await page.mouse.move(outTgt.x, outTgt.y, { steps: 8 });
await page.waitForTimeout(150);
const invalidLine = await q('.miso-flow__connection-path.invalid');
await page.mouse.up();
await page.waitForTimeout(400);
assert('invalid connection line shown', invalidLine === 1);
assert('validator refused source->out', await q('.miso-flow__edge') === edges0);

// --- 1b. connection line endpoint tracks the cursor (regression:
// XYHandle reports `to` in pane coordinates; after fit-view the
// viewport is translated, so an unconverted endpoint sits ~t away)
const src1 = center(await page.locator('.miso-flow__handle.source[data-nodeid="lfo"]').boundingBox());
const empty = { x: src1.x + 170, y: src1.y - 120 }; // no handle nearby
await page.mouse.move(src1.x, src1.y);
await page.mouse.down();
await page.mouse.move(empty.x, empty.y, { steps: 6 });
await page.waitForTimeout(150);
const lineEnd = await page.evaluate(() => {
  const d = document.querySelector('.miso-flow__connection-path').getAttribute('d');
  const pair = d.trim().split(' ').pop().split(',');
  const flow = { x: parseFloat(pair[0]), y: parseFloat(pair[1]) };
  const t = new DOMMatrixReadOnly(
    getComputedStyle(document.querySelector('.xyflow__viewport')).transform);
  const box = document.querySelector('.miso-flow').getBoundingClientRect();
  return { x: flow.x * t.a + t.e + box.left, y: flow.y * t.d + t.f + box.top };
});
await page.mouse.up();
await page.waitForTimeout(300);
assert('connection line ends under the cursor',
  Math.abs(lineEnd.x - empty.x) < 3 && Math.abs(lineEnd.y - empty.y) < 3,
  `cursor ${JSON.stringify(empty)} line ${JSON.stringify(lineEnd)}`);

// --- 2. reconnection: select the filt->delay cable, drag its target anchor to out
await page.evaluate(() => {
  document.querySelector('.miso-flow__edge-simplebezier .miso-flow__edge-interaction')
    .dispatchEvent(new MouseEvent('click', { bubbles: true }));
});
await page.waitForTimeout(400);
assert('edge selected shows 2 anchors', await q('.miso-flow__edge-anchor') === 2);
assert('edge toolbar visible', await q('.miso-flow__edge-toolbar button') === 1);

const dBefore = await page.evaluate(() =>
  document.querySelector('.miso-flow__edge-simplebezier .miso-flow__edge-path').getAttribute('d'));
const anchor = center(await page.locator('.miso-flow__edge-anchor-target[data-edgeid="c-filt-delay"]').boundingBox());
await page.mouse.move(anchor.x, anchor.y);
await page.mouse.down();
await page.mouse.move(outTgt.x, outTgt.y, { steps: 10 });
await page.mouse.up();
await page.waitForTimeout(500);
const dAfter = await page.evaluate(() =>
  document.querySelector('.miso-flow__edge-simplebezier .miso-flow__edge-path')?.getAttribute('d'));
assert('reconnected: path endpoint moved', dAfter && dAfter !== dBefore,
  `${dBefore} -> ${dAfter}`);
assert('reconnect preserved edge count', await q('.miso-flow__edge') === edges0);

// --- 3. valid connect: osc-a -> delay
const delayTgt = center(await page.locator('.miso-flow__handle.target[data-nodeid="delay"]').boundingBox());
await page.mouse.move(oscSrc.x, oscSrc.y);
await page.mouse.down();
await page.mouse.move(delayTgt.x, delayTgt.y, { steps: 8 });
await page.mouse.up();
await page.waitForTimeout(400);
assert('valid connect accepted', await q('.miso-flow__edge') === edges0 + 1);

// --- 4. edge toolbar unpatch
await page.evaluate(() => {
  document.querySelector('.miso-flow__edge-smoothstep .miso-flow__edge-interaction')
    .dispatchEvent(new MouseEvent('click', { bubbles: true }));
});
await page.waitForTimeout(400);
const edgesBeforeUnpatch = await q('.miso-flow__edge');
await page.locator('.miso-flow__edge-toolbar button').click();
await page.waitForTimeout(400);
assert('unpatch removed the cable', await q('.miso-flow__edge') === edgesBeforeUnpatch - 1);

// --- 10b. hide / unhide, non-deletable flag, drag handle, exact zoom, framing
await page.click('.miso-flow__controls button:nth-child(4)'); // fit (controls grew)
await page.waitForTimeout(600);

// hide the delay via its toolbar; its cables disappear with it
const edgesBeforeHide = await q('.miso-flow__edge');
await page.locator('.miso-flow__node[data-id="delay"] .pw-grip').click();
await page.waitForTimeout(300);
await page.locator('.miso-flow__node-toolbar button', { hasText: 'hide' }).click();
await page.waitForTimeout(400);
assert('hidden module has no visible card',
  await page.evaluate(() =>
    document.querySelector('.miso-flow__node[data-id="delay"]').offsetParent === null));
assert('edges to hidden module are gone', await q('.miso-flow__edge') < edgesBeforeHide,
  `${edgesBeforeHide} -> ${await q('.miso-flow__edge')}`);
await page.locator('.pw-toggle', { hasText: 'hidden' }).click();
await page.waitForTimeout(400);
assert('show-hidden restores the module',
  await page.evaluate(() =>
    document.querySelector('.miso-flow__node[data-id="delay"]').offsetParent !== null));
assert('edges restored with it', await q('.miso-flow__edge') === edgesBeforeHide);

// the main out refuses deletion: no delete button, Delete key is a no-op
await page.locator('.miso-flow__node[data-id="out"]').click();
await page.waitForTimeout(300);
assert('non-deletable module has no delete button',
  await page.evaluate(() =>
    [...document.querySelectorAll('.miso-flow__node-toolbar button')]
      .every((b) => b.textContent !== 'delete')));
const nodesBeforeOut = await q('.miso-flow__node');
await page.keyboard.press('Delete');
await page.waitForTimeout(400);
assert('delete key respects nodeDeletable', await q('.miso-flow__node') === nodesBeforeOut);
await page.mouse.click(720, 830);
await page.waitForTimeout(300);

// delay only drags by its grip
const dt0 = await page.evaluate(() =>
  document.querySelector('.miso-flow__node[data-id="delay"]').style.transform);
const dcard = await page.locator('.miso-flow__node[data-id="delay"] .pw-name').boundingBox();
await page.mouse.move(dcard.x + 10, dcard.y + 5);
await page.mouse.down();
await page.mouse.move(dcard.x + 70, dcard.y + 45, { steps: 5 });
await page.mouse.up();
await page.waitForTimeout(400);
assert('card body does not drag (dragHandle)',
  await page.evaluate(() =>
    document.querySelector('.miso-flow__node[data-id="delay"]').style.transform) === dt0);
const grip = await page.locator('.miso-flow__node[data-id="delay"] .pw-grip').boundingBox();
await page.mouse.move(grip.x + grip.width / 2, grip.y + grip.height / 2);
await page.mouse.down();
await page.mouse.move(grip.x + 60, grip.y + 40, { steps: 5 });
await page.mouse.up();
await page.waitForTimeout(400);
assert('grip drags the module (dragHandle)',
  await page.evaluate(() =>
    document.querySelector('.miso-flow__node[data-id="delay"]').style.transform) !== dt0);

// exact zoom via 1:1 control
await page.click('.miso-flow__controls button:nth-child(3)');
await page.waitForTimeout(600);
assert('1:1 control sets zoom 100%',
  (await page.locator('.pw-status').textContent()).includes('zoom 100%'));

// frame the group from its toolbar
const vpBeforeFrame = await page.evaluate(() =>
  document.querySelector('.xyflow__viewport').style.transform);
const vg = await page.locator('.miso-flow__node[data-id="voice"]').boundingBox();
await page.mouse.click(vg.x + vg.width - 30, vg.y + 12);
await page.waitForTimeout(300);
await page.locator('.miso-flow__node-toolbar button', { hasText: 'frame' }).click();
await page.waitForTimeout(600);
assert('frame button fits the group',
  await page.evaluate(() =>
    document.querySelector('.xyflow__viewport').style.transform) !== vpBeforeFrame);
await page.mouse.click(720, 830);
await page.waitForTimeout(300);


// --- 5. keyboard delete: select delay, press Delete
const nodesBefore = await q('.miso-flow__node');
await page.locator('.miso-flow__node[data-id="delay"]').click();
await page.waitForTimeout(300);
await page.keyboard.press('Delete');
await page.waitForTimeout(500);
assert('delete key removed the node', await q('.miso-flow__node') === nodesBefore - 1);

// --- 6. minimap click recenters
const vp1 = await page.evaluate(() => document.querySelector('.xyflow__viewport').style.transform);
const mmBox = await page.locator('.miso-flow__minimap svg').boundingBox();
await page.mouse.click(mmBox.x + mmBox.width * 0.25, mmBox.y + mmBox.height * 0.3);
await page.waitForTimeout(700);
const vp2 = await page.evaluate(() => document.querySelector('.xyflow__viewport').style.transform);
assert('minimap click recentered viewport', vp2 !== vp1, `${vp1} -> ${vp2}`);

// --- 7. snap to grid toggle affects dragging
await page.click('.pw-toggle');
await page.waitForTimeout(300);
assert('snap toggle turns on', await q('.pw-toggle.on') === 1);
const osc = page.locator('.miso-flow__node[data-id="osc-a"]');
let ob = await osc.boundingBox();
await page.mouse.move(ob.x + ob.width / 2, ob.y + 10);
await page.mouse.down();
await page.mouse.move(ob.x + ob.width / 2 + 93, ob.y + 10 + 57, { steps: 6 });
await page.mouse.up();
await page.waitForTimeout(500);
const snapped = await page.evaluate(() => {
  const t = document.querySelector('.miso-flow__node[data-id="osc-a"]').style.transform;
  const m = t.match(/translate\(([-\d.]+)px, ([-\d.]+)px\)/);
  return { x: parseFloat(m[1]), y: parseFloat(m[2]) };
});
assert('drag position snapped to 20px grid',
  Math.abs(snapped.x % 20) < 0.01 && Math.abs(snapped.y % 20) < 0.01,
  JSON.stringify(snapped));

// --- 8. subflow: dragging the Voice group carries its children
await page.click('.miso-flow__controls button:nth-child(4)'); // re-fit for a known layout
await page.waitForTimeout(600);
const envBefore = await page.evaluate(() =>
  document.querySelector('.miso-flow__node[data-id="env"]').style.transform);
const group = await page.locator('.miso-flow__node[data-id="voice"]').boundingBox();
const gp = { x: group.x + group.width - 30, y: group.y + 12 }; // empty top-right strip
await page.mouse.move(gp.x, gp.y);
await page.mouse.down();
await page.mouse.move(gp.x + 60, gp.y + 40, { steps: 6 });
await page.mouse.up();
await page.waitForTimeout(500);
const envAfter = await page.evaluate(() =>
  document.querySelector('.miso-flow__node[data-id="env"]').style.transform);
assert('group drag moved child with it', envAfter !== envBefore, `${envBefore} -> ${envAfter}`);

// --- 9. multi-handle: the cv handle exists with its own id
assert('named cv handle present', await q('.miso-flow__handle[data-handleid="cv"]') === 1);

// --- 10. node toolbar delete + resizer on selection
await page.click('.miso-flow__controls button:nth-child(4)');
await page.waitForTimeout(600);
await page.locator('.miso-flow__node[data-id="lfo"]').click();
await page.waitForTimeout(400);
assert('node toolbar on selection', await q('.miso-flow__node-toolbar button') === 2);
assert('resize controls on selection',
  await q('.miso-flow__node[data-id="lfo"] .miso-flow__resize-control') === 8);
const rb = await page.locator('.miso-flow__node[data-id="lfo"] .miso-flow__resize-control.handle.bottom-right').boundingBox();
const lfoW = (await page.locator('.miso-flow__node[data-id="lfo"]').boundingBox()).width;
await page.mouse.move(rb.x + rb.width / 2, rb.y + rb.height / 2);
await page.mouse.down();
await page.mouse.move(rb.x + rb.width / 2 + 50, rb.y + rb.height / 2 + 20, { steps: 5 });
await page.mouse.up();
await page.waitForTimeout(400);
const lfoW2 = (await page.locator('.miso-flow__node[data-id="lfo"]').boundingBox()).width;
assert('module resized', lfoW2 > lfoW + 20, `${lfoW} -> ${lfoW2}`);
const nodesBeforeTb = await q('.miso-flow__node');
await page.locator('.miso-flow__node-toolbar button', { hasText: 'delete' }).click();
await page.waitForTimeout(400);
assert('node toolbar delete works', await q('.miso-flow__node') === nodesBeforeTb - 1);

// --- 11. ctrl-click multi-select + group drag of two modules
await page.locator('.miso-flow__node[data-id="osc-a"]').click();
await page.keyboard.down('Control');
await page.locator('.miso-flow__node[data-id="out"]').click();
await page.keyboard.up('Control');
await page.waitForTimeout(400);
assert('ctrl-click builds multi-selection', await q('.miso-flow__node.selected') === 2);
const outT1 = await page.evaluate(() =>
  document.querySelector('.miso-flow__node[data-id="out"]').style.transform);
const oa = await page.locator('.miso-flow__node[data-id="osc-a"]').boundingBox();
await page.mouse.move(oa.x + oa.width / 2, oa.y + oa.height / 2);
await page.mouse.down();
await page.mouse.move(oa.x + oa.width / 2 + 40, oa.y + oa.height / 2 + 30, { steps: 5 });
await page.mouse.up();
await page.waitForTimeout(400);
const outT2 = await page.evaluate(() =>
  document.querySelector('.miso-flow__node[data-id="out"]').style.transform);
assert('multi-selection drags as a group', outT2 !== outT1);

// --- 12a. palette: add modules at runtime
const beforeAdd = await q('.miso-flow__node');
await page.locator('.pw-palette button', { hasText: 'Add oscillator' }).click();
await page.waitForTimeout(400);
await page.locator('.pw-palette button', { hasText: 'Add effect' }).click();
await page.waitForTimeout(400);
assert('palette adds modules', await q('.miso-flow__node') === beforeAdd + 2,
  `${beforeAdd} -> ${await q('.miso-flow__node')}`);
assert('added module has handles', await q('.miso-flow__node[data-id="effect-1"] .miso-flow__handle') === 2);
assert('status line live', (await page.locator('.pw-status').textContent()).includes('modules'));

// delete the effect, add another: the fresh id must not collide
await page.locator('.miso-flow__node[data-id="effect-1"]').click();
await page.waitForTimeout(300);
await page.keyboard.press('Delete');
await page.waitForTimeout(400);
await page.locator('.pw-palette button', { hasText: 'Add effect' }).click();
await page.waitForTimeout(400);
assert('fresh id after delete + re-add', await q('.miso-flow__node[data-id="effect-1"]') === 1);

// --- 12. shift-drag selection box
await page.mouse.click(720, 820); // clear selection
await page.waitForTimeout(300);
await page.keyboard.down('Shift');
await page.mouse.move(10, 10);
await page.mouse.down();
await page.mouse.move(1430, 880, { steps: 6 });
const boxMid = await q('.miso-flow__selection');
await page.mouse.up();
await page.keyboard.up('Shift');
await page.waitForTimeout(400);
assert('selection box renders', boxMid === 1);
assert('selection box selected modules', await q('.miso-flow__node.selected') >= 2,
  `got ${await q('.miso-flow__node.selected')}`);

await page.screenshot({ path: process.env.SHOT || 'interact4.png' });
console.log(failures === 0 ? 'ALL PASS' : `${failures} FAILURES`);
console.log('--- console ---');
for (const m of msgs.slice(0, 15)) console.log(m);
await browser.close();
process.exit(failures === 0 ? 0 : 1);
