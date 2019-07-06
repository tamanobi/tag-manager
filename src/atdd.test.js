describe('トップページ', () => {
  beforeAll(async () => {
    await page.goto('http://localhost:62449');
  });

  it('should contain "タグ一覧"', async () => {
    await expect(page).toMatch('タグ一覧')
  });

  it('should contain "カテゴリ一覧"', async () => {
    await expect(page).toMatch('カテゴリ一覧')
  });
});