import { expect, type Locator, type Page } from "@playwright/test";

export type Viewport = { name: string; width: number; height: number; touch: boolean };

/** The viewport matrix from the responsive UI spec. `touch` means a coarse pointer. */
export const VIEWPORTS: Viewport[] = [
  { name: "small-android", width: 360, height: 740, touch: true },
  { name: "iphone", width: 390, height: 844, touch: true },
  { name: "landscape-phone", width: 740, height: 360, touch: true },
  { name: "tablet", width: 768, height: 1024, touch: true },
  { name: "desktop", width: 1280, height: 800, touch: false },
];

/** With this prefix `uniqueName` returns the maximum 24 characters. */
export const LONG_NAME_PREFIX = "longplayername";

/** Fails if the page's context is not the viewport the test asked for (e.g. a silent desktop fallback). */
export async function expectViewport(page: Page, vp: Viewport): Promise<void> {
  const env = await page.evaluate(() => ({
    width: window.innerWidth,
    height: window.innerHeight,
    coarse: window.matchMedia("(pointer: coarse)").matches,
  }));
  expect(env, "context does not match the requested viewport").toEqual({
    width: vp.width,
    height: vp.height,
    coarse: vp.touch,
  });
}

/**
 * Compares against the configured viewport width, not `innerWidth`: with `isMobile`
 * an overflowing page zooms out and `innerWidth` grows with it.
 */
export async function expectNoHorizontalScroll(page: Page): Promise<void> {
  const width = page.viewportSize()!.width;
  const scrollWidth = await page.evaluate(() => document.documentElement.scrollWidth);
  expect(scrollWidth, "page scrolls horizontally").toBeLessThanOrEqual(width);
}

export async function boxOf(locator: Locator) {
  const box = await locator.boundingBox();
  expect(box, `${locator} has no layout box`).not.toBeNull();
  return box!;
}

/** Every matched element must be at least 44×44 CSS px. */
export async function expectTapTargets(locator: Locator): Promise<void> {
  const elements = await locator.all();
  expect(elements.length, `${locator} matched nothing`).toBeGreaterThan(0);
  for (const element of elements) {
    const box = await boxOf(element);
    expect(box.height, `${element} height`).toBeGreaterThanOrEqual(44);
    expect(box.width, `${element} width`).toBeGreaterThanOrEqual(44);
  }
}
