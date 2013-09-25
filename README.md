Small and lightweight http server based on Mochoweb for iOS in app verification.
=====


Send POST request on **/production** or **/sandbox** path with receipt data in request body.

***Note***: Request body must contain receipt data **"as is"** without any changes

**Response** - something like this:

**{"status": {"code": "0", "status": "This receipt is real."}}**

More possible status codes and descriptions see in **iap_manager.erl**
