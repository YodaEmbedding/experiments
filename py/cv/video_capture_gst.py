import cv2

# char* inputPipe = "nvcamerasrc fpsRange="30 30" intent=3 ! nvvidconv flip-method=6 ! 'video/x-raw(memory:NVMM), width=(int)1920, height=(int)1080, format=(string)I420, framerate=(fraction)30/1' ! videoconvert ! appsink";
# char* outputPipe = "appsrc ! videoconvert ! omxh264enc control-rate=2 bitrate=4000000 ! 'video/x-h264, stream-format=(string)byte-stream' ! 264parse ! rtph264pay mtu=1400 ! udpsink host=$CLIENT_IP port=5000 sync=false async=false";

# cap = cv2.VideoCapture(0)
# cap = cv2.VideoCapture("autovideosrc ! appsink")
# cap = cv2.VideoCapture('autovideosrc ! decodebin ! videoconvert ! appsink')
cap = cv2.VideoCapture("v4l2src ! appsink")

while True:
    ret, frame = cap.read()
    # gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    print(ret)
    print(frame if frame is None else frame.shape)
    cv2.imshow("frame", frame)
    if cv2.waitKey(1) & 0xFF == ord("q"):
        break

cap.release()
cv2.destroyAllWindows()
