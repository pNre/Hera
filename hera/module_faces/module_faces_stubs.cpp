#define CAML_NAME_SPACE

#include <stdio.h>
#include <string>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>
#include <opencv2/opencv.hpp>
#include "FaceDetector.h"
#include "KeyPointDetector.h"

using namespace cv;
using namespace std;

extern "C"
{
  CAMLprim value
  mapFaceLandmarkRect(vector<cv::Point2f> faceLandmarks, int start, int end)
  {
    CAMLparam0();

    float min_x = faceLandmarks[start].x;
    float max_x = faceLandmarks[start].x;
    float min_y = faceLandmarks[start].y;
    float max_y = faceLandmarks[start].y;

    for (size_t j = start + 1; j <= end; j++)
    {
      min_x = min(min_x, faceLandmarks[j].x);
      max_x = max(max_x, faceLandmarks[j].x);
      min_y = min(min_y, faceLandmarks[j].y);
      max_y = max(max_y, faceLandmarks[j].y);
    }

    CAMLlocal1(rect_left);
    rect_left = caml_alloc(4, 0);
    Store_field(rect_left, 0, Val_int(round(min_x)));
    Store_field(rect_left, 1, Val_int(round(min_y)));
    Store_field(rect_left, 2, Val_int(round(max_x - min_x)));
    Store_field(rect_left, 3, Val_int(round(max_y - min_y)));
    CAMLreturn(rect_left);
  }

  CAMLprim value
  caml_detect_faces(value photo_path, value face_config_path, value face_weights_path, value eyes_model_path)
  {
    CAMLparam4(photo_path, face_config_path, face_weights_path, eyes_model_path);
    CAMLlocal2(faces_cli, faces_cons);

    char *_face_config_path, *_face_weights_path, *_eyes_model_path, *_photo_path;

    _face_config_path = caml_stat_strdup(String_val(face_config_path));
    _face_weights_path = caml_stat_strdup(String_val(face_weights_path));
    _eyes_model_path = caml_stat_strdup(String_val(eyes_model_path));
    _photo_path = caml_stat_strdup(String_val(photo_path));
    caml_release_runtime_system();

    FaceDetector faceDetector(_face_config_path, _face_weights_path);
    KeyPointDetector keypointDetector(_eyes_model_path);

    Mat image = imread(_photo_path);
    auto faceRects = faceDetector.detect_face_rectangles(image);
    auto landmarks = keypointDetector.detect_key_points(faceRects, image);

    caml_stat_free(_face_config_path);
    caml_stat_free(_face_weights_path);
    caml_stat_free(_eyes_model_path);
    caml_stat_free(_photo_path);
    caml_acquire_runtime_system();

    faces_cli = Val_emptylist;

    for (std::size_t i = 0; i < faceRects.size(); i++)
    {
      CAMLlocal2(landmarks_cli, eyes_cons);
      landmarks_cli = Val_emptylist;

      auto faceLandmarks = landmarks[i];

      if (faceLandmarks.size() == 68)
      {
        // drawPolyline(im, faceLandmarks, 0, 16);           // Jaw line
        // drawPolyline(im, faceLandmarks, 17, 21);          // Left eyebrow
        // drawPolyline(im, faceLandmarks, 22, 26);          // Right eyebrow
        // drawPolyline(im, faceLandmarks, 27, 30);          // Nose bridge
        // drawPolyline(im, faceLandmarks, 30, 35, true);    // Lower nose
        // drawPolyline(im, faceLandmarks, 36, 41, true);    // Left eye
        // drawPolyline(im, faceLandmarks, 42, 47, true);    // Right Eye
        // drawPolyline(im, faceLandmarks, 48, 59, true);    // Outer lip
        // drawPolyline(im, faceLandmarks, 60, 67, true);    // Inner lip

        eyes_cons = caml_alloc(2, 0);
        Store_field(eyes_cons, 0, mapFaceLandmarkRect(faceLandmarks, 17, 21));
        Store_field(eyes_cons, 1, landmarks_cli);
        landmarks_cli = eyes_cons;

        eyes_cons = caml_alloc(2, 0);
        Store_field(eyes_cons, 0, mapFaceLandmarkRect(faceLandmarks, 22, 26));
        Store_field(eyes_cons, 1, landmarks_cli);
        landmarks_cli = eyes_cons;

        eyes_cons = caml_alloc(2, 0);
        Store_field(eyes_cons, 0, mapFaceLandmarkRect(faceLandmarks, 48, 59));
        Store_field(eyes_cons, 1, landmarks_cli);
        landmarks_cli = eyes_cons;
      }

      CAMLlocal1(face_rect);
      face_rect = caml_alloc(4, 0);
      Store_field(face_rect, 0, Val_int(faceRects[i].x));
      Store_field(face_rect, 1, Val_int(faceRects[i].y));
      Store_field(face_rect, 2, Val_int(faceRects[i].width));
      Store_field(face_rect, 3, Val_int(faceRects[i].height));

      CAMLlocal1(face_eyes_pair);
      face_eyes_pair = caml_alloc(2, 0);
      Store_field(face_eyes_pair, 0, face_rect);
      Store_field(face_eyes_pair, 1, landmarks_cli);

      faces_cons = caml_alloc(2, 0);
      Store_field(faces_cons, 0, face_eyes_pair);
      Store_field(faces_cons, 1, faces_cli);
      faces_cli = faces_cons;
    }

    CAMLreturn(faces_cli);
  }
}