
import { Cl, cvToValue } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const admin = accounts.get("deployer")!;
const user = accounts.get("wallet_2")!;
const recipient = accounts.get("wallet_3")!;
const contract = "bault";

const readOnly = (fn: string, args: any[], sender = admin) =>
  simnet.callReadOnlyFn(contract, fn, args, sender).result;

const unwrapClarityJson = (value: any): any => {
  if (value === null || value === undefined) {
    return value;
  }

  if (Array.isArray(value)) {
    return value.map(unwrapClarityJson);
  }

  if (typeof value !== "object") {
    return value;
  }

  if ("type" in value && "value" in value) {
    switch (value.type) {
      case "uint":
      case "int":
        return BigInt(value.value);
      case "bool":
        return value.value;
      case "tuple": {
        const tupleValue = value.value as Record<string, any>;
        const result: Record<string, any> = {};
        for (const [key, entry] of Object.entries(tupleValue)) {
          result[key] = unwrapClarityJson(entry);
        }
        return result;
      }
      case "list":
        return (value.value as any[]).map(unwrapClarityJson);
      case "optional":
        return value.value === null ? null : unwrapClarityJson(value.value);
      case "response":
        return unwrapClarityJson(value.value);
      default:
        return value.value;
    }
  }

  const result: Record<string, any> = {};
  for (const [key, entry] of Object.entries(value)) {
    result[key] = unwrapClarityJson(entry);
  }
  return result;
};

const readOnlyValue = (fn: string, args: any[], sender = admin) =>
  unwrapClarityJson(cvToValue(readOnly(fn, args, sender)));

const callPublic = (fn: string, args: any[], sender = admin) =>
  simnet.callPublicFn(contract, fn, args, sender).result;

const getVaultInfo = () =>
  readOnlyValue("get-vault-info", []) as Record<string, any>;

const getVaultHealth = () =>
  readOnlyValue("get-vault-health", []) as Record<string, any>;

const getUserShares = (who: string) =>
  readOnlyValue("get-user-shares", [Cl.principal(who)]) as Record<string, any>;

const ensureUserHasShares = (minShares: bigint) => {
  const whitelistResult = callPublic(
    "add-to-whitelist",
    [Cl.principal(user)],
    admin
  );
  expect(whitelistResult).toBeOk(Cl.bool(true));

  let userData = getUserShares(user);
  if (userData.shares >= minShares) {
    return userData;
  }

  const depositAmount = 1_000n;
  const preInfo = getVaultInfo();
  const preShares = preInfo["total-shares"] as bigint;
  const preAssets = preInfo["total-assets"] as bigint;
  const expectedShares =
    preShares === 0n ? depositAmount : (depositAmount * preShares) / preAssets;

  const depositResult = callPublic(
    "deposit",
    [Cl.uint(depositAmount)],
    user
  );
  expect(depositResult).toBeOk(Cl.uint(expectedShares));

  userData = getUserShares(user);
  return userData;
};

describe("bault core flows", () => {
  it("initializes with admin defaults", () => {
    const info = getVaultInfo();
    expect(info["total-shares"]).toBe(0n);
    expect(info["total-assets"]).toBe(0n);
    expect(info["paused"]).toBe(false);
    expect(info["emergency-mode"]).toBe(false);
    expect(info["multi-asset-enabled"]).toBe(false);

    const adminWhitelisted = readOnlyValue("is-whitelisted", [
      Cl.principal(admin),
    ]);
    const userWhitelisted = readOnlyValue("is-whitelisted", [
      Cl.principal(user),
    ]);
    expect(adminWhitelisted).toBe(true);
    expect(userWhitelisted).toBe(false);
  });

  it("gates deposits by whitelist and mints shares", () => {
    const health = getVaultHealth();
    expect(health["invariants-valid"]).toBe(true);

    const amount = 1_000n;
    const notWhitelisted = callPublic("deposit", [Cl.uint(amount)], user);
    expect(notWhitelisted).toBeErr(Cl.uint(102));

    const whitelistResult = callPublic(
      "add-to-whitelist",
      [Cl.principal(user)],
      admin
    );
    expect(whitelistResult).toBeOk(Cl.bool(true));

    const preInfo = getVaultInfo();
    const preShares = preInfo["total-shares"] as bigint;
    const preAssets = preInfo["total-assets"] as bigint;
    const preUser = getUserShares(user);

    const expectedShares =
      preShares === 0n ? amount : (amount * preShares) / preAssets;
    const depositResult = callPublic("deposit", [Cl.uint(amount)], user);
    expect(depositResult).toBeOk(Cl.uint(expectedShares));

    const postInfo = getVaultInfo();
    expect(postInfo["total-shares"]).toBe(preShares + expectedShares);
    expect(postInfo["total-assets"]).toBe(preAssets + amount);

    const postUser = getUserShares(user);
    expect(postUser.shares).toBe(preUser.shares + expectedShares);
    expect(postUser["total-volume"]).toBe(preUser["total-volume"] + amount);
  });

  it("withdraws shares, applies fee, and updates totals", () => {
    const userData = ensureUserHasShares(2n);
    const preInfo = getVaultInfo();
    const preShares = preInfo["total-shares"] as bigint;
    const preAssets = preInfo["total-assets"] as bigint;

    const withdrawShares =
      userData.shares > 1n ? userData.shares / 2n : userData.shares;

    const feePreview = readOnlyValue("get-withdraw-fee-preview", [
      Cl.principal(user),
      Cl.uint(withdrawShares),
    ]) as bigint;
    const assetsToRedeem = (withdrawShares * preAssets) / preShares;
    const expectedNet =
      assetsToRedeem >= feePreview ? assetsToRedeem - feePreview : 0n;

    const withdrawResult = callPublic(
      "withdraw",
      [Cl.uint(withdrawShares)],
      user
    );
    expect(withdrawResult).toBeOk(Cl.uint(expectedNet));

    const postInfo = getVaultInfo();
    expect(postInfo["total-shares"]).toBe(preShares - withdrawShares);
    expect(postInfo["total-assets"]).toBe(preAssets - assetsToRedeem);

    const postUser = getUserShares(user);
    expect(postUser.shares).toBe(userData.shares - withdrawShares);
  });

  it("enforces slippage protections", () => {
    ensureUserHasShares(1n);

    const depositAmount = 100n;
    const previewShares = readOnlyValue("preview-deposit", [
      Cl.uint(depositAmount),
    ]) as bigint;
    const badDeposit = callPublic(
      "deposit-with-slippage",
      [Cl.uint(depositAmount), Cl.uint(previewShares + 1n)],
      user
    );
    expect(badDeposit).toBeErr(Cl.uint(1006));

    const userData = getUserShares(user);
    const withdrawShares = userData.shares > 1n ? 1n : userData.shares;
    const previewWithdraw = readOnlyValue("preview-withdraw-with-fee", [
      Cl.principal(user),
      Cl.uint(withdrawShares),
    ]) as Record<string, any>;
    const minAssets = (previewWithdraw["net-amount"] as bigint) + 1n;

    const badWithdraw = callPublic(
      "withdraw-with-slippage",
      [Cl.uint(withdrawShares), Cl.uint(minAssets)],
      user
    );
    expect(badWithdraw).toBeErr(Cl.uint(1006));
  });

  it("blocks operations while paused", () => {
    const pauseResult = callPublic("pause", [], admin);
    expect(pauseResult).toBeOk(Cl.bool(true));

    const amount = 10n;
    const blockedDeposit = callPublic("deposit", [Cl.uint(amount)], user);
    expect(blockedDeposit).toBeErr(Cl.uint(101));

    const unpauseResult = callPublic("unpause", [], admin);
    expect(unpauseResult).toBeOk(Cl.bool(true));
  });

  it("transfers shares between users", () => {
    const userData = ensureUserHasShares(1n);
    const recipientData = getUserShares(recipient);
    const preInfo = getVaultInfo();

    const transferResult = callPublic(
      "transfer",
      [Cl.principal(recipient), Cl.uint(1n)],
      user
    );
    expect(transferResult).toBeOk(Cl.bool(true));

    const postUser = getUserShares(user);
    const postRecipient = getUserShares(recipient);
    const postInfo = getVaultInfo();

    expect(postUser.shares).toBe(userData.shares - 1n);
    expect(postRecipient.shares).toBe(recipientData.shares + 1n);
    expect(postInfo["total-shares"]).toBe(preInfo["total-shares"]);
  });

  it("rejects emergency withdraws when not enabled", () => {
    const result = callPublic(
      "emergency-withdraw",
      [Cl.principal(recipient)],
      user
    );
    expect(result).toBeErr(Cl.uint(108));
  });
});
