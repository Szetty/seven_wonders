import { expect, test } from "@playwright/test";

test("the mobile project runs on a 412px touch screen, in fixture and manual contexts", async ({
  page,
  browser,
}) => {
  test.skip(test.info().project.name !== "mobile", "guards the mobile project only");
  const manual = await browser.newContext();
  try {
    for (const target of [page, await manual.newPage()]) {
      await target.goto("/login");
      const env = await target.evaluate(() => ({
        width: window.innerWidth,
        coarse: window.matchMedia("(pointer: coarse)").matches,
      }));
      expect(env).toEqual({ width: 412, coarse: true });
    }
  } finally {
    await manual.close();
  }
});
