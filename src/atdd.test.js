describe("トップページ", () => {
  beforeAll(async () => {
    await page.goto("http://localhost:62449");
  });

  it('should contain "タグ一覧"', async () => {
    await expect(page).toMatch("タグ一覧");
  });

  it('should contain "カテゴリ一覧"', async () => {
    await expect(page).toMatch("カテゴリ一覧");
  });
});

describe("トップページでのクリック", () => {
  beforeEach(async () => {
    await page.goto("http://localhost:62449");
  });

  it('should move "タグ編集画面"', async () => {
    const tag = "鹿目まどか";
    await expect(page).toClick("a", { text: tag });
    await expect(page).toMatch(`タグ編集画面(${tag})`);
  });

  it('should return "タグ編集画面" -> "トップページ"', async () => {
    const tag = "鹿目まどか";
    await expect(page).toClick("a", { text: tag });
    await expect(page).toClick("button", { text: "back to top" });
    await expect(page).toMatch("タグ一覧");
  });
});
