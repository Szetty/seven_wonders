export type TestContext = {
	cleanUps: (() => Promise<any>)[]
}
export let testCtx;

beforeEach(async () => {
	testCtx = {
		cleanUps: []
	}
});

afterEach(async () => {
	await testCtx.cleanUps.forEach(async (cleanUp) => await cleanUp());
});