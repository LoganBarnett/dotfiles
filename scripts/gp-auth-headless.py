#!/usr/bin/env python3
"""
Headless browser automation for GlobalProtect SSO authentication.
Handles username/password and TOTP/MFA challenges.
"""

import sys
import os
import time
from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeout

def log(message):
    """Log with timestamp to stderr."""
    print(f"[gp-auth] {message}", file=sys.stderr, flush=True)

def authenticate(auth_url, username, password, totp_code):
    """
    Complete SSO authentication in headless browser.

    Args:
        auth_url: The authentication URL from gpclient (http://127.0.0.1:PORT/UUID)
        username: User email address
        password: User password
        totp_code: Current TOTP code

    Returns:
        True if successful, False otherwise
    """
    # Extract the base callback URL from the initial auth URL.
    # This is used to detect when we've returned to the local gpclient server.
    import re
    callback_pattern = re.match(r'(http://(?:127\.0\.0\.1|localhost):\d+)', auth_url)
    callback_base = callback_pattern.group(1) if callback_pattern else None
    log(f"Starting headless browser automation")
    log(f"Auth URL: {auth_url}")
    log(f"Callback base: {callback_base}")
    log(f"Username: {username}")

    with sync_playwright() as p:
        # Launch browser with args to avoid detection
        browser = p.chromium.launch(
            headless=False,  # Use headed mode to avoid detection
            args=[
                '--disable-blink-features=AutomationControlled',
                '--no-sandbox'
            ]
        )
        context = browser.new_context(
            viewport={'width': 1280, 'height': 720},
            user_agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36',
            locale='en-US',
            timezone_id='America/New_York',
            # Add more realistic browser context
            permissions=['geolocation'],
            has_touch=False,
            is_mobile=False,
        )

        # Override navigator.webdriver to avoid detection
        page = context.new_page()
        page.add_init_script("""
            Object.defineProperty(navigator, 'webdriver', {
                get: () => undefined
            });
        """)

        try:
            # Navigate to auth URL
            log("Navigating to authentication page...")
            page.goto(auth_url, wait_until='networkidle', timeout=30000)
            log(f"Current URL: {page.url}")

            # Wait a moment for page to fully load
            time.sleep(2)

            # Try to find and fill username field
            log("Looking for username field...")
            username_selectors = [
                'input[name="username"]',
                'input[name="email"]',
                'input[type="email"]',
                'input[id="username"]',
                'input[id="email"]',
                'input[placeholder*="email" i]',
                'input[placeholder*="username" i]',
                'input[type="text"]'
            ]

            username_filled = False
            for selector in username_selectors:
                try:
                    if page.locator(selector).count() > 0:
                        log(f"Found username field with selector: {selector}")
                        page.fill(selector, username)
                        log("Username filled")
                        username_filled = True
                        break
                except Exception as e:
                    continue

            if not username_filled:
                log("WARNING: Could not find username field")
                log(f"Page content: {page.content()[:500]}")

            # Try to find and fill password field
            log("Looking for password field...")
            password_selectors = [
                'input[name="password"]',
                'input[type="password"]',
                'input[id="password"]'
            ]

            password_filled = False
            for selector in password_selectors:
                try:
                    if page.locator(selector).count() > 0:
                        log(f"Found password field with selector: {selector}")
                        page.fill(selector, password)
                        log("Password filled")
                        password_filled = True
                        break
                except Exception as e:
                    continue

            if not password_filled:
                log("WARNING: Could not find password field")

            # Take screenshot for debugging
            screenshot_path = "/tmp/gp-auth-before-submit.png"
            page.screenshot(path=screenshot_path)
            log(f"Screenshot saved to {screenshot_path}")

            # Try to submit the form
            log("Looking for submit button...")
            submit_selectors = [
                'button[type="submit"]',
                'input[type="submit"]',
                'button:has-text("Sign in")',
                'button:has-text("Login")',
                'button:has-text("Continue")',
                'button:has-text("Next")',
                'input[value="Sign in"]',
                'input[value="Login"]'
            ]

            submitted = False
            for selector in submit_selectors:
                try:
                    if page.locator(selector).count() > 0:
                        log(f"Found submit button with selector: {selector}")
                        page.click(selector)
                        log("Clicked submit button")
                        submitted = True
                        break
                except Exception as e:
                    continue

            if not submitted:
                log("WARNING: Could not find submit button, trying form submission...")
                try:
                    page.keyboard.press("Enter")
                    log("Pressed Enter to submit")
                except Exception as e:
                    log(f"Could not submit form: {e}")

            # Wait a moment for initial response
            log("Waiting for initial response after submit...")
            time.sleep(3)

            # Take screenshot right after submit
            screenshot_path = "/tmp/gp-auth-after-submit.png"
            page.screenshot(path=screenshot_path)
            log(f"Screenshot after submit saved to {screenshot_path}")

            log(f"Current URL after submit: {page.url}")

            # Look for TOTP/MFA field
            log("Looking for TOTP/MFA field...")
            totp_selectors = [
                'input[name="answer"]',
                'input[name="code"]',
                'input[name="otp"]',
                'input[name="token"]',
                'input[name="mfa"]',
                'input[type="text"][inputmode="numeric"]',
                'input[placeholder*="code" i]',
                'input[id="code"]',
                'input[id="otp"]',
                'input[id="token"]'
            ]

            totp_filled = False
            for selector in totp_selectors:
                try:
                    # Wait a bit for the field to appear
                    page.wait_for_selector(selector, timeout=5000)
                    if page.locator(selector).count() > 0:
                        log(f"Found TOTP field with selector: {selector}")
                        page.fill(selector, totp_code)
                        log("TOTP code filled")
                        totp_filled = True

                        # Take screenshot
                        screenshot_path = "/tmp/gp-auth-totp-filled.png"
                        page.screenshot(path=screenshot_path)
                        log(f"Screenshot saved to {screenshot_path}")

                        # Submit TOTP form
                        for submit_sel in submit_selectors:
                            try:
                                if page.locator(submit_sel).count() > 0:
                                    log(f"Clicking submit after TOTP: {submit_sel}")
                                    page.click(submit_sel)
                                    break
                            except:
                                continue
                        break
                except PlaywrightTimeout:
                    continue
                except Exception as e:
                    log(f"Error with selector {selector}: {e}")
                    continue

            if not totp_filled:
                log("WARNING: Could not find TOTP field - it may not be required")

            # Wait for callback to complete
            log("Waiting for authentication callback to complete...")
            for i in range(30):
                current_url = page.url
                log(f"Current URL [{i}]: {current_url}")

                # Check if we've returned to the local gpclient server.
                if callback_base and current_url.startswith(callback_base):
                    log(f"Detected callback to gpclient server: {current_url}")
                    time.sleep(2)  # Give it a moment to complete
                    break

                # Check if we've been redirected to success page or callback
                if 'success' in current_url.lower() or 'callback' in current_url.lower() or 'localhost' in current_url or '127.0.0.1' in current_url:
                    log("Detected callback/success URL!")
                    time.sleep(2)  # Give it a moment to complete
                    break

                # Wait for page to be in a stable state before checking content
                try:
                    page.wait_for_load_state('networkidle', timeout=2000)
                except:
                    # If timeout, page is still loading, continue waiting
                    time.sleep(1)
                    continue

                # Check for "Authentication Complete" text on the page
                try:
                    page_content = page.content()
                except Exception as e:
                    log(f"Could not get page content (page may be navigating): {e}")
                    time.sleep(1)
                    continue

                if 'Authentication Complete' in page_content or 'authentication complete' in page_content.lower():
                    log("Detected 'Authentication Complete' message!")

                    # Look for the globalprotectcallback URL
                    try:
                        log("Looking for callback URL to extract...")

                        # Try to find the "click here" link with the callback URL
                        callback_url = None
                        link_selectors = [
                            'a:has-text("click here")',
                            'a:text("click here")',
                            'a[href*="globalprotectcallback"]',
                            'a[href^="globalprotect"]'
                        ]

                        for selector in link_selectors:
                            try:
                                if page.locator(selector).count() > 0:
                                    href = page.locator(selector).first.get_attribute('href')
                                    if href and 'globalprotect' in href.lower():
                                        callback_url = href
                                        log(f"Found callback URL with selector: {selector}")
                                        log(f"Callback URL: {callback_url}")
                                        break
                            except:
                                continue

                        if callback_url:
                            # Print ONLY the callback URL to stdout (functional output)
                            print(callback_url, flush=True)
                            log("Callback URL extracted and sent to stdout")
                        else:
                            log("WARNING: Could not find callback URL in page")
                            log("Page may use different callback mechanism")

                    except Exception as e:
                        log(f"Error extracting callback URL: {e}")
                        time.sleep(2)

                    break

                time.sleep(1)

            log("Authentication flow completed")

            # Final screenshot
            screenshot_path = "/tmp/gp-auth-final.png"
            page.screenshot(path=screenshot_path)
            log(f"Final screenshot saved to {screenshot_path}")

            return True

        except Exception as e:
            log(f"ERROR during authentication: {e}")
            try:
                screenshot_path = "/tmp/gp-auth-error.png"
                page.screenshot(path=screenshot_path)
                log(f"Error screenshot saved to {screenshot_path}")
            except:
                pass
            return False

        finally:
            browser.close()

def main():
    if len(sys.argv) != 5:
        print("Usage: gp-auth-headless.py <auth_url> <username> <password> <totp_code>")
        sys.exit(1)

    auth_url = sys.argv[1]
    username = sys.argv[2]
    password = sys.argv[3]
    totp_code = sys.argv[4]

    success = authenticate(auth_url, username, password, totp_code)
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
