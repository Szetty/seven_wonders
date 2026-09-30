import { expect, test } from "@playwright/test";
import { VIEWPORTS, expectNoHorizontalScroll, expectViewport } from "./support/layout";

for (const vp of VIEWPORTS) {
  test.describe(`${vp.name} ${vp.width}x${vp.height}`, () => {
    test.use({
      viewport: { width: vp.width, height: vp.height },
      hasTouch: vp.touch,
      isMobile: vp.touch,
    });

    test("login fits", async ({ page }) => {
      await page.goto("/login");
      await expectViewport(page, vp);
      await expectNoHorizontalScroll(page);
      await expect(page.locator("#login-submit")).toBeVisible();
    });
  });
}
